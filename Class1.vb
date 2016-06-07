Imports System.ComponentModel
Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Drawing.Imaging
Imports System.Drawing.Text
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Windows.Forms

MustInherit Class ThemeContainer154
    Inherits ContainerControl

#Region " Initialization "

    Protected G As Graphics, B As Bitmap

    Sub New()
        SetStyle(DirectCast(139270, ControlStyles), True)

        _ImageSize = Size.Empty
        Font = New Font("Verdana", 8S)

        MeasureBitmap = New Bitmap(1, 1)
        MeasureGraphics = Graphics.FromImage(MeasureBitmap)

        DrawRadialPath = New GraphicsPath

        InvalidateCustimization()
    End Sub

    Protected NotOverridable Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        If DoneCreation Then InitializeMessages()

        InvalidateCustimization()
        ColorHook()

        If Not _LockWidth = 0 Then Width = _LockWidth
        If Not _LockHeight = 0 Then Height = _LockHeight
        If Not _ControlMode Then MyBase.Dock = DockStyle.Fill

        Transparent = _Transparent
        If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

        MyBase.OnHandleCreated(e)
    End Sub

    Private DoneCreation As Boolean
    Protected NotOverridable Overrides Sub OnParentChanged(ByVal e As EventArgs)
        MyBase.OnParentChanged(e)

        If Parent Is Nothing Then Return
        _IsParentForm = TypeOf Parent Is Form

        If Not _ControlMode Then
            InitializeMessages()

            If _IsParentForm Then
                ParentForm.FormBorderStyle = _BorderStyle
                ParentForm.TransparencyKey = _TransparencyKey

                If Not DesignMode Then
                    AddHandler ParentForm.Shown, AddressOf FormShown
                End If
            End If

            Parent.BackColor = BackColor
        End If

        OnCreation()
        DoneCreation = True
        InvalidateTimer()
    End Sub

#End Region

    Private Sub DoAnimation(ByVal i As Boolean)
        OnAnimation()
        If i Then Invalidate()
    End Sub

    Protected NotOverridable Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        If Width = 0 OrElse Height = 0 Then Return

        If _Transparent AndAlso _ControlMode Then
            PaintHook()
            e.Graphics.DrawImage(B, 0, 0)
        Else
            G = e.Graphics
            PaintHook()
        End If
    End Sub

    Protected Overrides Sub OnHandleDestroyed(ByVal e As EventArgs)
        RemoveAnimationCallback(AddressOf DoAnimation)
        MyBase.OnHandleDestroyed(e)
    End Sub

    Private HasShown As Boolean
    Private Sub FormShown(ByVal sender As Object, ByVal e As EventArgs)
        If _ControlMode OrElse HasShown Then Return

        If _StartPosition = FormStartPosition.CenterParent OrElse _StartPosition = FormStartPosition.CenterScreen Then
            Dim SB As Rectangle = Screen.PrimaryScreen.Bounds
            Dim CB As Rectangle = ParentForm.Bounds
            ParentForm.Location = New Point(SB.Width \ 2 - CB.Width \ 2, SB.Height \ 2 - CB.Width \ 2)
        End If

        HasShown = True
    End Sub


#Region " Size Handling "

    Private Frame As Rectangle
    Protected NotOverridable Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If _Movable AndAlso Not _ControlMode Then
            Frame = New Rectangle(7, 7, Width - 14, _Header - 7)
        End If

        InvalidateBitmap()
        Invalidate()

        MyBase.OnSizeChanged(e)
    End Sub

    Protected Overrides Sub SetBoundsCore(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal specified As BoundsSpecified)
        If Not _LockWidth = 0 Then width = _LockWidth
        If Not _LockHeight = 0 Then height = _LockHeight
        MyBase.SetBoundsCore(x, y, width, height, specified)
    End Sub

#End Region

#Region " State Handling "

    Protected State As MouseState
    Private Sub SetState(ByVal current As MouseState)
        State = current
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseMove(ByVal e As MouseEventArgs)
        If Not (_IsParentForm AndAlso ParentForm.WindowState = FormWindowState.Maximized) Then
            If _Sizable AndAlso Not _ControlMode Then InvalidateMouse()
        End If

        MyBase.OnMouseMove(e)
    End Sub

    Protected Overrides Sub OnEnabledChanged(ByVal e As EventArgs)
        If Enabled Then SetState(MouseState.None) Else SetState(MouseState.Block)
        MyBase.OnEnabledChanged(e)
    End Sub

    Protected Overrides Sub OnMouseEnter(ByVal e As EventArgs)
        SetState(MouseState.Over)
        MyBase.OnMouseEnter(e)
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As MouseEventArgs)
        SetState(MouseState.Over)
        MyBase.OnMouseUp(e)
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        SetState(MouseState.None)

        If GetChildAtPoint(PointToClient(MousePosition)) IsNot Nothing Then
            If _Sizable AndAlso Not _ControlMode Then
                Cursor = Cursors.Default
                Previous = 0
            End If
        End If

        MyBase.OnMouseLeave(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then SetState(MouseState.Down)

        If Not (_IsParentForm AndAlso ParentForm.WindowState = FormWindowState.Maximized OrElse _ControlMode) Then
            If _Movable AndAlso Frame.Contains(e.Location) Then
                Capture = False
                WM_LMBUTTONDOWN = True
                DefWndProc(Messages(0))
            ElseIf _Sizable AndAlso Not Previous = 0 Then
                Capture = False
                WM_LMBUTTONDOWN = True
                DefWndProc(Messages(Previous))
            End If
        End If

        MyBase.OnMouseDown(e)
    End Sub

    Private WM_LMBUTTONDOWN As Boolean
    Protected Overrides Sub WndProc(ByRef m As Message)
        MyBase.WndProc(m)

        If WM_LMBUTTONDOWN AndAlso m.Msg = 513 Then
            WM_LMBUTTONDOWN = False

            SetState(MouseState.Over)
            If Not _SmartBounds Then Return

            If IsParentMdi Then
                CorrectBounds(New Rectangle(Point.Empty, Parent.Parent.Size))
            Else
                CorrectBounds(Screen.FromControl(Parent).WorkingArea)
            End If
        End If
    End Sub

    Private GetIndexPoint As Point
    Private B1, B2, B3, B4 As Boolean
    Private Function GetIndex() As Integer
        GetIndexPoint = PointToClient(MousePosition)
        B1 = GetIndexPoint.X < 7
        B2 = GetIndexPoint.X > Width - 7
        B3 = GetIndexPoint.Y < 7
        B4 = GetIndexPoint.Y > Height - 7

        If B1 AndAlso B3 Then Return 4
        If B1 AndAlso B4 Then Return 7
        If B2 AndAlso B3 Then Return 5
        If B2 AndAlso B4 Then Return 8
        If B1 Then Return 1
        If B2 Then Return 2
        If B3 Then Return 3
        If B4 Then Return 6
        Return 0
    End Function

    Private Current, Previous As Integer
    Private Sub InvalidateMouse()
        Current = GetIndex()
        If Current = Previous Then Return

        Previous = Current
        Select Case Previous
            Case 0
                Cursor = Cursors.Default
            Case 1, 2
                Cursor = Cursors.SizeWE
            Case 3, 6
                Cursor = Cursors.SizeNS
            Case 4, 8
                Cursor = Cursors.SizeNWSE
            Case 5, 7
                Cursor = Cursors.SizeNESW
        End Select
    End Sub

    Private Messages(8) As Message
    Private Sub InitializeMessages()
        Messages(0) = Message.Create(Parent.Handle, 161, New IntPtr(2), IntPtr.Zero)
        For I As Integer = 1 To 8
            Messages(I) = Message.Create(Parent.Handle, 161, New IntPtr(I + 9), IntPtr.Zero)
        Next
    End Sub

    Private Sub CorrectBounds(ByVal bounds As Rectangle)
        If Parent.Width > bounds.Width Then Parent.Width = bounds.Width
        If Parent.Height > bounds.Height Then Parent.Height = bounds.Height

        Dim X As Integer = Parent.Location.X
        Dim Y As Integer = Parent.Location.Y

        If X < bounds.X Then X = bounds.X
        If Y < bounds.Y Then Y = bounds.Y

        Dim Width As Integer = bounds.X + bounds.Width
        Dim Height As Integer = bounds.Y + bounds.Height

        If X + Parent.Width > Width Then X = Width - Parent.Width
        If Y + Parent.Height > Height Then Y = Height - Parent.Height

        Parent.Location = New Point(X, Y)
    End Sub

#End Region


#Region " Base Properties "

    Overrides Property Dock As DockStyle
        Get
            Return MyBase.Dock
        End Get
        Set(ByVal value As DockStyle)
            If Not _ControlMode Then Return
            MyBase.Dock = value
        End Set
    End Property

    Private _BackColor As Boolean
    <Category("Misc")>
    Overrides Property BackColor() As Color
        Get
            Return MyBase.BackColor
        End Get
        Set(ByVal value As Color)
            If value = MyBase.BackColor Then Return

            If Not IsHandleCreated AndAlso _ControlMode AndAlso value = Color.Transparent Then
                _BackColor = True
                Return
            End If

            MyBase.BackColor = value
            If Parent IsNot Nothing Then
                If Not _ControlMode Then Parent.BackColor = value
                ColorHook()
            End If
        End Set
    End Property

    Overrides Property MinimumSize As Size
        Get
            Return MyBase.MinimumSize
        End Get
        Set(ByVal value As Size)
            MyBase.MinimumSize = value
            If Parent IsNot Nothing Then Parent.MinimumSize = value
        End Set
    End Property

    Overrides Property MaximumSize As Size
        Get
            Return MyBase.MaximumSize
        End Get
        Set(ByVal value As Size)
            MyBase.MaximumSize = value
            If Parent IsNot Nothing Then Parent.MaximumSize = value
        End Set
    End Property

    Overrides Property Text() As String
        Get
            Return MyBase.Text
        End Get
        Set(ByVal value As String)
            MyBase.Text = value
            Invalidate()
        End Set
    End Property

    Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Invalidate()
        End Set
    End Property

    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property ForeColor() As Color
        Get
            Return Color.Empty
        End Get
        Set(ByVal value As Color)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property BackgroundImage() As Image
        Get
            Return Nothing
        End Get
        Set(ByVal value As Image)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property BackgroundImageLayout() As ImageLayout
        Get
            Return ImageLayout.None
        End Get
        Set(ByVal value As ImageLayout)
        End Set
    End Property

#End Region

#Region " Public Properties "

    Private _SmartBounds As Boolean = True
    Property SmartBounds() As Boolean
        Get
            Return _SmartBounds
        End Get
        Set(ByVal value As Boolean)
            _SmartBounds = value
        End Set
    End Property

    Private _Movable As Boolean = True
    Property Movable() As Boolean
        Get
            Return _Movable
        End Get
        Set(ByVal value As Boolean)
            _Movable = value
        End Set
    End Property

    Private _Sizable As Boolean = True
    Property Sizable() As Boolean
        Get
            Return _Sizable
        End Get
        Set(ByVal value As Boolean)
            _Sizable = value
        End Set
    End Property

    Private _TransparencyKey As Color
    Property TransparencyKey() As Color
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.TransparencyKey Else Return _TransparencyKey
        End Get
        Set(ByVal value As Color)
            If value = _TransparencyKey Then Return
            _TransparencyKey = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.TransparencyKey = value
                ColorHook()
            End If
        End Set
    End Property

    Private _BorderStyle As FormBorderStyle
    Property BorderStyle() As FormBorderStyle
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.FormBorderStyle Else Return _BorderStyle
        End Get
        Set(ByVal value As FormBorderStyle)
            _BorderStyle = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.FormBorderStyle = value

                If Not value = FormBorderStyle.None Then
                    Movable = False
                    Sizable = False
                End If
            End If
        End Set
    End Property

    Private _StartPosition As FormStartPosition
    Property StartPosition As FormStartPosition
        Get
            If _IsParentForm AndAlso Not _ControlMode Then Return ParentForm.StartPosition Else Return _StartPosition
        End Get
        Set(ByVal value As FormStartPosition)
            _StartPosition = value

            If _IsParentForm AndAlso Not _ControlMode Then
                ParentForm.StartPosition = value
            End If
        End Set
    End Property

    Private _NoRounding As Boolean
    Property NoRounding() As Boolean
        Get
            Return _NoRounding
        End Get
        Set(ByVal v As Boolean)
            _NoRounding = v
            Invalidate()
        End Set
    End Property

    Private _Image As Image
    Property Image() As Image
        Get
            Return _Image
        End Get
        Set(ByVal value As Image)
            If value Is Nothing Then _ImageSize = Size.Empty Else _ImageSize = value.Size

            _Image = value
            Invalidate()
        End Set
    End Property

    Private Items As New Dictionary(Of String, Color)
    Property Colors() As Bloom()
        Get
            Dim T As New List(Of Bloom)
            Dim E As Dictionary(Of String, Color).Enumerator = Items.GetEnumerator

            While E.MoveNext
                T.Add(New Bloom(E.Current.Key, E.Current.Value))
            End While

            Return T.ToArray
        End Get
        Set(ByVal value As Bloom())
            For Each B As Bloom In value
                If Items.ContainsKey(B.Name) Then Items(B.Name) = B.Value
            Next

            InvalidateCustimization()
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Customization As String
    Property Customization() As String
        Get
            Return _Customization
        End Get
        Set(ByVal value As String)
            If value = _Customization Then Return

            Dim Data As Byte()
            Dim Items As Bloom() = Colors

            Try
                Data = Convert.FromBase64String(value)
                For I As Integer = 0 To Items.Length - 1
                    Items(I).Value = Color.FromArgb(BitConverter.ToInt32(Data, I * 4))
                Next
            Catch
                Return
            End Try

            _Customization = value

            Colors = Items
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Transparent As Boolean
    Property Transparent() As Boolean
        Get
            Return _Transparent
        End Get
        Set(ByVal value As Boolean)
            _Transparent = value
            If Not (IsHandleCreated OrElse _ControlMode) Then Return

            If Not value AndAlso Not BackColor.A = 255 Then
                Throw New Exception("Unable to change value to false while a transparent BackColor is in use.")
            End If

            SetStyle(ControlStyles.Opaque, Not value)
            SetStyle(ControlStyles.SupportsTransparentBackColor, value)

            InvalidateBitmap()
            Invalidate()
        End Set
    End Property

#End Region

#Region " Private Properties "

    Private _ImageSize As Size
    Protected ReadOnly Property ImageSize() As Size
        Get
            Return _ImageSize
        End Get
    End Property

    Private _IsParentForm As Boolean
    Protected ReadOnly Property IsParentForm As Boolean
        Get
            Return _IsParentForm
        End Get
    End Property

    Protected ReadOnly Property IsParentMdi As Boolean
        Get
            If Parent Is Nothing Then Return False
            Return Parent.Parent IsNot Nothing
        End Get
    End Property

    Private _LockWidth As Integer
    Protected Property LockWidth() As Integer
        Get
            Return _LockWidth
        End Get
        Set(ByVal value As Integer)
            _LockWidth = value
            If Not LockWidth = 0 AndAlso IsHandleCreated Then Width = LockWidth
        End Set
    End Property

    Private _LockHeight As Integer
    Protected Property LockHeight() As Integer
        Get
            Return _LockHeight
        End Get
        Set(ByVal value As Integer)
            _LockHeight = value
            If Not LockHeight = 0 AndAlso IsHandleCreated Then Height = LockHeight
        End Set
    End Property

    Private _Header As Integer = 24
    Protected Property Header() As Integer
        Get
            Return _Header
        End Get
        Set(ByVal v As Integer)
            _Header = v

            If Not _ControlMode Then
                Frame = New Rectangle(7, 7, Width - 14, v - 7)
                Invalidate()
            End If
        End Set
    End Property

    Private _ControlMode As Boolean
    Protected Property ControlMode() As Boolean
        Get
            Return _ControlMode
        End Get
        Set(ByVal v As Boolean)
            _ControlMode = v

            Transparent = _Transparent
            If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

            InvalidateBitmap()
            Invalidate()
        End Set
    End Property

    Private _IsAnimated As Boolean
    Protected Property IsAnimated() As Boolean
        Get
            Return _IsAnimated
        End Get
        Set(ByVal value As Boolean)
            _IsAnimated = value
            InvalidateTimer()
        End Set
    End Property

#End Region


#Region " Property Helpers "

    Protected Function GetPen(ByVal name As String) As Pen
        Return New Pen(Items(name))
    End Function
    Protected Function GetPen(ByVal name As String, ByVal width As Single) As Pen
        Return New Pen(Items(name), width)
    End Function

    Protected Function GetBrush(ByVal name As String) As SolidBrush
        Return New SolidBrush(Items(name))
    End Function

    Protected Function GetColor(ByVal name As String) As Color
        Return Items(name)
    End Function

    Protected Sub SetColor(ByVal name As String, ByVal value As Color)
        If Items.ContainsKey(name) Then Items(name) = value Else Items.Add(name, value)
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(a, r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal value As Color)
        SetColor(name, Color.FromArgb(a, value))
    End Sub

    Private Sub InvalidateBitmap()
        If _Transparent AndAlso _ControlMode Then
            If Width = 0 OrElse Height = 0 Then Return
            B = New Bitmap(Width, Height, PixelFormat.Format32bppPArgb)
            G = Graphics.FromImage(B)
        Else
            G = Nothing
            B = Nothing
        End If
    End Sub

    Private Sub InvalidateCustimization()
        Dim M As New MemoryStream(Items.Count * 4)

        For Each B As Bloom In Colors
            M.Write(BitConverter.GetBytes(B.Value.ToArgb), 0, 4)
        Next

        M.Close()
        _Customization = Convert.ToBase64String(M.ToArray)
    End Sub

    Private Sub InvalidateTimer()
        If DesignMode OrElse Not DoneCreation Then Return

        If _IsAnimated Then
            AddAnimationCallback(AddressOf DoAnimation)
        Else
            RemoveAnimationCallback(AddressOf DoAnimation)
        End If
    End Sub

#End Region


#Region " User Hooks "

    Protected MustOverride Sub ColorHook()
    Protected MustOverride Sub PaintHook()

    Protected Overridable Sub OnCreation()
    End Sub

    Protected Overridable Sub OnAnimation()
    End Sub

#End Region


#Region " Offset "

    Private OffsetReturnRectangle As Rectangle
    Protected Function Offset(ByVal r As Rectangle, ByVal amount As Integer) As Rectangle
        OffsetReturnRectangle = New Rectangle(r.X + amount, r.Y + amount, r.Width - (amount * 2), r.Height - (amount * 2))
        Return OffsetReturnRectangle
    End Function

    Private OffsetReturnSize As Size
    Protected Function Offset(ByVal s As Size, ByVal amount As Integer) As Size
        OffsetReturnSize = New Size(s.Width + amount, s.Height + amount)
        Return OffsetReturnSize
    End Function

    Private OffsetReturnPoint As Point
    Protected Function Offset(ByVal p As Point, ByVal amount As Integer) As Point
        OffsetReturnPoint = New Point(p.X + amount, p.Y + amount)
        Return OffsetReturnPoint
    End Function

#End Region

#Region " Center "

    Private CenterReturn As Point

    Protected Function Center(ByVal p As Rectangle, ByVal c As Rectangle) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X + c.X, (p.Height \ 2 - c.Height \ 2) + p.Y + c.Y)
        Return CenterReturn
    End Function
    Protected Function Center(ByVal p As Rectangle, ByVal c As Size) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X, (p.Height \ 2 - c.Height \ 2) + p.Y)
        Return CenterReturn
    End Function

    Protected Function Center(ByVal child As Rectangle) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal child As Size) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal childWidth As Integer, ByVal childHeight As Integer) As Point
        Return Center(Width, Height, childWidth, childHeight)
    End Function

    Protected Function Center(ByVal p As Size, ByVal c As Size) As Point
        Return Center(p.Width, p.Height, c.Width, c.Height)
    End Function

    Protected Function Center(ByVal pWidth As Integer, ByVal pHeight As Integer, ByVal cWidth As Integer, ByVal cHeight As Integer) As Point
        CenterReturn = New Point(pWidth \ 2 - cWidth \ 2, pHeight \ 2 - cHeight \ 2)
        Return CenterReturn
    End Function

#End Region

#Region " Measure "

    Private MeasureBitmap As Bitmap
    Private MeasureGraphics As Graphics

    Protected Function Measure() As Size
        SyncLock MeasureGraphics
            Return MeasureGraphics.MeasureString(Text, Font, Width).ToSize
        End SyncLock
    End Function
    Protected Function Measure(ByVal text As String) As Size
        SyncLock MeasureGraphics
            Return MeasureGraphics.MeasureString(text, Font, Width).ToSize
        End SyncLock
    End Function

#End Region


#Region " DrawPixel "

    Private DrawPixelBrush As SolidBrush

    Protected Sub DrawPixel(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer)
        If _Transparent Then
            B.SetPixel(x, y, c1)
        Else
            DrawPixelBrush = New SolidBrush(c1)
            G.FillRectangle(DrawPixelBrush, x, y, 1, 1)
        End If
    End Sub

#End Region

#Region " DrawCorners "

    Private DrawCornersBrush As SolidBrush

    Protected Sub DrawCorners(ByVal c1 As Color, ByVal offset As Integer)
        DrawCorners(c1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle, ByVal offset As Integer)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawCorners(c1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawCorners(ByVal c1 As Color)
        DrawCorners(c1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        If _NoRounding Then Return

        If _Transparent Then
            B.SetPixel(x, y, c1)
            B.SetPixel(x + (width - 1), y, c1)
            B.SetPixel(x, y + (height - 1), c1)
            B.SetPixel(x + (width - 1), y + (height - 1), c1)
        Else
            DrawCornersBrush = New SolidBrush(c1)
            G.FillRectangle(DrawCornersBrush, x, y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x, y + (height - 1), 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y + (height - 1), 1, 1)
        End If
    End Sub

#End Region

#Region " DrawBorders "

    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal offset As Integer)
        DrawBorders(p1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle, ByVal offset As Integer)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawBorders(p1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawBorders(ByVal p1 As Pen)
        DrawBorders(p1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        G.DrawRectangle(p1, x, y, width - 1, height - 1)
    End Sub

#End Region

#Region " DrawText "

    Private DrawTextPoint As Point
    Private DrawTextSize As Size

    Protected Sub DrawText(ByVal b1 As Brush, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawText(b1, Text, a, x, y)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal text As String, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If text.Length = 0 Then Return

        DrawTextSize = Measure(text)
        DrawTextPoint = New Point(Width \ 2 - DrawTextSize.Width \ 2, Header \ 2 - DrawTextSize.Height \ 2)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawString(text, Font, b1, x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Center
                G.DrawString(text, Font, b1, DrawTextPoint.X + x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Right
                G.DrawString(text, Font, b1, Width - DrawTextSize.Width - x, DrawTextPoint.Y + y)
        End Select
    End Sub

    Protected Sub DrawText(ByVal b1 As Brush, ByVal p1 As Point)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, p1)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal x As Integer, ByVal y As Integer)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, x, y)
    End Sub

#End Region

#Region " DrawImage "

    Private DrawImagePoint As Point

    Protected Sub DrawImage(ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, a, x, y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        DrawImagePoint = New Point(Width \ 2 - image.Width \ 2, Header \ 2 - image.Height \ 2)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawImage(image, x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Center
                G.DrawImage(image, DrawImagePoint.X + x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Right
                G.DrawImage(image, Width - image.Width - x, DrawImagePoint.Y + y, image.Width, image.Height)
        End Select
    End Sub

    Protected Sub DrawImage(ByVal p1 As Point)
        DrawImage(_Image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, x, y)
    End Sub

    Protected Sub DrawImage(ByVal image As Image, ByVal p1 As Point)
        DrawImage(image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        G.DrawImage(image, x, y, image.Width, image.Height)
    End Sub

#End Region

#Region " DrawGradient "

    Private DrawGradientBrush As LinearGradientBrush
    Private DrawGradientRectangle As Rectangle

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, 90.0F)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, angle)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub


    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, angle)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub

#End Region

#Region " DrawRadial "

    Private DrawRadialPath As GraphicsPath
    Private DrawRadialBrush1 As PathGradientBrush
    Private DrawRadialBrush2 As LinearGradientBrush
    Private DrawRadialRectangle As Rectangle

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, width \ 2, height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal center As Point)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, cx, cy)
    End Sub

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawRadial(blend, r, r.Width \ 2, r.Height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal center As Point)
        DrawRadial(blend, r, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialPath.Reset()
        DrawRadialPath.AddEllipse(r.X, r.Y, r.Width - 1, r.Height - 1)

        DrawRadialBrush1 = New PathGradientBrush(DrawRadialPath)
        DrawRadialBrush1.CenterPoint = New Point(r.X + cx, r.Y + cy)
        DrawRadialBrush1.InterpolationColors = blend

        If G.SmoothingMode = SmoothingMode.AntiAlias Then
            G.FillEllipse(DrawRadialBrush1, r.X + 1, r.Y + 1, r.Width - 3, r.Height - 3)
        Else
            G.FillEllipse(DrawRadialBrush1, r)
        End If
    End Sub


    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, angle)
        G.FillEllipse(DrawGradientBrush, r)
    End Sub

#End Region

#Region " CreateRound "

    Private CreateRoundPath As GraphicsPath
    Private CreateRoundRectangle As Rectangle

    Function CreateRound(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal slope As Integer) As GraphicsPath
        CreateRoundRectangle = New Rectangle(x, y, width, height)
        Return CreateRound(CreateRoundRectangle, slope)
    End Function

    Function CreateRound(ByVal r As Rectangle, ByVal slope As Integer) As GraphicsPath
        CreateRoundPath = New GraphicsPath(FillMode.Winding)
        CreateRoundPath.AddArc(r.X, r.Y, slope, slope, 180.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Y, slope, slope, 270.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Bottom - slope, slope, slope, 0.0F, 90.0F)
        CreateRoundPath.AddArc(r.X, r.Bottom - slope, slope, slope, 90.0F, 90.0F)
        CreateRoundPath.CloseFigure()
        Return CreateRoundPath
    End Function

#End Region

End Class

MustInherit Class ThemeControl154
    Inherits Control


#Region " Initialization "

    Protected G As Graphics, B As Bitmap

    Sub New()
        SetStyle(DirectCast(139270, ControlStyles), True)

        _ImageSize = Size.Empty
        Font = New Font("Verdana", 8S)

        MeasureBitmap = New Bitmap(1, 1)
        MeasureGraphics = Graphics.FromImage(MeasureBitmap)

        DrawRadialPath = New GraphicsPath

        InvalidateCustimization() 'Remove?
    End Sub

    Protected NotOverridable Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        InvalidateCustimization()
        ColorHook()

        If Not _LockWidth = 0 Then Width = _LockWidth
        If Not _LockHeight = 0 Then Height = _LockHeight

        Transparent = _Transparent
        If _Transparent AndAlso _BackColor Then BackColor = Color.Transparent

        MyBase.OnHandleCreated(e)
    End Sub

    Private DoneCreation As Boolean
    Protected NotOverridable Overrides Sub OnParentChanged(ByVal e As EventArgs)
        If Parent IsNot Nothing Then
            OnCreation()
            DoneCreation = True
            InvalidateTimer()
        End If

        MyBase.OnParentChanged(e)
    End Sub

#End Region

    Private Sub DoAnimation(ByVal i As Boolean)
        OnAnimation()
        If i Then Invalidate()
    End Sub

    Protected NotOverridable Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        If Width = 0 OrElse Height = 0 Then Return

        If _Transparent Then
            PaintHook()
            e.Graphics.DrawImage(B, 0, 0)
        Else
            G = e.Graphics
            PaintHook()
        End If
    End Sub

    Protected Overrides Sub OnHandleDestroyed(ByVal e As EventArgs)
        RemoveAnimationCallback(AddressOf DoAnimation)
        MyBase.OnHandleDestroyed(e)
    End Sub

#Region " Size Handling "

    Protected NotOverridable Overrides Sub OnSizeChanged(ByVal e As EventArgs)
        If _Transparent Then
            InvalidateBitmap()
        End If

        Invalidate()
        MyBase.OnSizeChanged(e)
    End Sub

    Protected Overrides Sub SetBoundsCore(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal specified As BoundsSpecified)
        If Not _LockWidth = 0 Then width = _LockWidth
        If Not _LockHeight = 0 Then height = _LockHeight
        MyBase.SetBoundsCore(x, y, width, height, specified)
    End Sub

#End Region

#Region " State Handling "

    Private InPosition As Boolean
    Protected Overrides Sub OnMouseEnter(ByVal e As EventArgs)
        InPosition = True
        SetState(MouseState.Over)
        MyBase.OnMouseEnter(e)
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As MouseEventArgs)
        If InPosition Then SetState(MouseState.Over)
        MyBase.OnMouseUp(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Left Then SetState(MouseState.Down)
        MyBase.OnMouseDown(e)
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        InPosition = False
        SetState(MouseState.None)
        MyBase.OnMouseLeave(e)
    End Sub

    Protected Overrides Sub OnEnabledChanged(ByVal e As EventArgs)
        If Enabled Then SetState(MouseState.None) Else SetState(MouseState.Block)
        MyBase.OnEnabledChanged(e)
    End Sub

    Protected State As MouseState
    Private Sub SetState(ByVal current As MouseState)
        State = current
        Invalidate()
    End Sub

#End Region


#Region " Base Properties "

    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property ForeColor() As Color
        Get
            Return Color.Empty
        End Get
        Set(ByVal value As Color)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property BackgroundImage() As Image
        Get
            Return Nothing
        End Get
        Set(ByVal value As Image)
        End Set
    End Property
    <Browsable(False), EditorBrowsable(EditorBrowsableState.Never), DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)>
    Overrides Property BackgroundImageLayout() As ImageLayout
        Get
            Return ImageLayout.None
        End Get
        Set(ByVal value As ImageLayout)
        End Set
    End Property

    Overrides Property Text() As String
        Get
            Return MyBase.Text
        End Get
        Set(ByVal value As String)
            MyBase.Text = value
            Invalidate()
        End Set
    End Property
    Overrides Property Font() As Font
        Get
            Return MyBase.Font
        End Get
        Set(ByVal value As Font)
            MyBase.Font = value
            Invalidate()
        End Set
    End Property

    Private _BackColor As Boolean
    <Category("Misc")>
    Overrides Property BackColor() As Color
        Get
            Return MyBase.BackColor
        End Get
        Set(ByVal value As Color)
            If Not IsHandleCreated AndAlso value = Color.Transparent Then
                _BackColor = True
                Return
            End If

            MyBase.BackColor = value
            If Parent IsNot Nothing Then ColorHook()
        End Set
    End Property

#End Region

#Region " Public Properties "

    Private _NoRounding As Boolean
    Property NoRounding() As Boolean
        Get
            Return _NoRounding
        End Get
        Set(ByVal v As Boolean)
            _NoRounding = v
            Invalidate()
        End Set
    End Property

    Private _Image As Image
    Property Image() As Image
        Get
            Return _Image
        End Get
        Set(ByVal value As Image)
            If value Is Nothing Then
                _ImageSize = Size.Empty
            Else
                _ImageSize = value.Size
            End If

            _Image = value
            Invalidate()
        End Set
    End Property

    Private _Transparent As Boolean
    Property Transparent() As Boolean
        Get
            Return _Transparent
        End Get
        Set(ByVal value As Boolean)
            _Transparent = value
            If Not IsHandleCreated Then Return

            If Not value AndAlso Not BackColor.A = 255 Then
                Throw New Exception("Unable to change value to false while a transparent BackColor is in use.")
            End If

            SetStyle(ControlStyles.Opaque, Not value)
            SetStyle(ControlStyles.SupportsTransparentBackColor, value)

            If value Then InvalidateBitmap() Else B = Nothing
            Invalidate()
        End Set
    End Property

    Private Items As New Dictionary(Of String, Color)
    Property Colors() As Bloom()
        Get
            Dim T As New List(Of Bloom)
            Dim E As Dictionary(Of String, Color).Enumerator = Items.GetEnumerator

            While E.MoveNext
                T.Add(New Bloom(E.Current.Key, E.Current.Value))
            End While

            Return T.ToArray
        End Get
        Set(ByVal value As Bloom())
            For Each B As Bloom In value
                If Items.ContainsKey(B.Name) Then Items(B.Name) = B.Value
            Next

            InvalidateCustimization()
            ColorHook()
            Invalidate()
        End Set
    End Property

    Private _Customization As String
    Property Customization() As String
        Get
            Return _Customization
        End Get
        Set(ByVal value As String)
            If value = _Customization Then Return

            Dim Data As Byte()
            Dim Items As Bloom() = Colors

            Try
                Data = Convert.FromBase64String(value)
                For I As Integer = 0 To Items.Length - 1
                    Items(I).Value = Color.FromArgb(BitConverter.ToInt32(Data, I * 4))
                Next
            Catch
                Return
            End Try

            _Customization = value

            Colors = Items
            ColorHook()
            Invalidate()
        End Set
    End Property

#End Region

#Region " Private Properties "

    Private _ImageSize As Size
    Protected ReadOnly Property ImageSize() As Size
        Get
            Return _ImageSize
        End Get
    End Property

    Private _LockWidth As Integer
    Protected Property LockWidth() As Integer
        Get
            Return _LockWidth
        End Get
        Set(ByVal value As Integer)
            _LockWidth = value
            If Not LockWidth = 0 AndAlso IsHandleCreated Then Width = LockWidth
        End Set
    End Property

    Private _LockHeight As Integer
    Protected Property LockHeight() As Integer
        Get
            Return _LockHeight
        End Get
        Set(ByVal value As Integer)
            _LockHeight = value
            If Not LockHeight = 0 AndAlso IsHandleCreated Then Height = LockHeight
        End Set
    End Property

    Private _IsAnimated As Boolean
    Protected Property IsAnimated() As Boolean
        Get
            Return _IsAnimated
        End Get
        Set(ByVal value As Boolean)
            _IsAnimated = value
            InvalidateTimer()
        End Set
    End Property

#End Region


#Region " Property Helpers "

    Protected Function GetPen(ByVal name As String) As Pen
        Return New Pen(Items(name))
    End Function
    Protected Function GetPen(ByVal name As String, ByVal width As Single) As Pen
        Return New Pen(Items(name), width)
    End Function

    Protected Function GetBrush(ByVal name As String) As SolidBrush
        Return New SolidBrush(Items(name))
    End Function

    Protected Function GetColor(ByVal name As String) As Color
        Return Items(name)
    End Function

    Protected Sub SetColor(ByVal name As String, ByVal value As Color)
        If Items.ContainsKey(name) Then Items(name) = value Else Items.Add(name, value)
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        SetColor(name, Color.FromArgb(a, r, g, b))
    End Sub
    Protected Sub SetColor(ByVal name As String, ByVal a As Byte, ByVal value As Color)
        SetColor(name, Color.FromArgb(a, value))
    End Sub

    Private Sub InvalidateBitmap()
        If Width = 0 OrElse Height = 0 Then Return
        B = New Bitmap(Width, Height, PixelFormat.Format32bppPArgb)
        G = Graphics.FromImage(B)
    End Sub

    Private Sub InvalidateCustimization()
        Dim M As New MemoryStream(Items.Count * 4)

        For Each B As Bloom In Colors
            M.Write(BitConverter.GetBytes(B.Value.ToArgb), 0, 4)
        Next

        M.Close()
        _Customization = Convert.ToBase64String(M.ToArray)
    End Sub

    Private Sub InvalidateTimer()
        If DesignMode OrElse Not DoneCreation Then Return

        If _IsAnimated Then
            AddAnimationCallback(AddressOf DoAnimation)
        Else
            RemoveAnimationCallback(AddressOf DoAnimation)
        End If
    End Sub
#End Region


#Region " User Hooks "

    Protected MustOverride Sub ColorHook()
    Protected MustOverride Sub PaintHook()

    Protected Overridable Sub OnCreation()
    End Sub

    Protected Overridable Sub OnAnimation()
    End Sub

#End Region


#Region " Offset "

    Private OffsetReturnRectangle As Rectangle
    Protected Function Offset(ByVal r As Rectangle, ByVal amount As Integer) As Rectangle
        OffsetReturnRectangle = New Rectangle(r.X + amount, r.Y + amount, r.Width - (amount * 2), r.Height - (amount * 2))
        Return OffsetReturnRectangle
    End Function

    Private OffsetReturnSize As Size
    Protected Function Offset(ByVal s As Size, ByVal amount As Integer) As Size
        OffsetReturnSize = New Size(s.Width + amount, s.Height + amount)
        Return OffsetReturnSize
    End Function

    Private OffsetReturnPoint As Point
    Protected Function Offset(ByVal p As Point, ByVal amount As Integer) As Point
        OffsetReturnPoint = New Point(p.X + amount, p.Y + amount)
        Return OffsetReturnPoint
    End Function

#End Region

#Region " Center "

    Private CenterReturn As Point

    Protected Function Center(ByVal p As Rectangle, ByVal c As Rectangle) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X + c.X, (p.Height \ 2 - c.Height \ 2) + p.Y + c.Y)
        Return CenterReturn
    End Function
    Protected Function Center(ByVal p As Rectangle, ByVal c As Size) As Point
        CenterReturn = New Point((p.Width \ 2 - c.Width \ 2) + p.X, (p.Height \ 2 - c.Height \ 2) + p.Y)
        Return CenterReturn
    End Function

    Protected Function Center(ByVal child As Rectangle) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal child As Size) As Point
        Return Center(Width, Height, child.Width, child.Height)
    End Function
    Protected Function Center(ByVal childWidth As Integer, ByVal childHeight As Integer) As Point
        Return Center(Width, Height, childWidth, childHeight)
    End Function

    Protected Function Center(ByVal p As Size, ByVal c As Size) As Point
        Return Center(p.Width, p.Height, c.Width, c.Height)
    End Function

    Protected Function Center(ByVal pWidth As Integer, ByVal pHeight As Integer, ByVal cWidth As Integer, ByVal cHeight As Integer) As Point
        CenterReturn = New Point(pWidth \ 2 - cWidth \ 2, pHeight \ 2 - cHeight \ 2)
        Return CenterReturn
    End Function

#End Region

#Region " Measure "

    Private MeasureBitmap As Bitmap
    Private MeasureGraphics As Graphics 'TODO: Potential issues during multi-threading.

    Protected Function Measure() As Size
        Return MeasureGraphics.MeasureString(Text, Font, Width).ToSize
    End Function
    Protected Function Measure(ByVal text As String) As Size
        Return MeasureGraphics.MeasureString(text, Font, Width).ToSize
    End Function

#End Region


#Region " DrawPixel "

    Private DrawPixelBrush As SolidBrush

    Protected Sub DrawPixel(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer)
        If _Transparent Then
            B.SetPixel(x, y, c1)
        Else
            DrawPixelBrush = New SolidBrush(c1)
            G.FillRectangle(DrawPixelBrush, x, y, 1, 1)
        End If
    End Sub

#End Region

#Region " DrawCorners "

    Private DrawCornersBrush As SolidBrush

    Protected Sub DrawCorners(ByVal c1 As Color, ByVal offset As Integer)
        DrawCorners(c1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle, ByVal offset As Integer)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height, offset)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawCorners(c1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawCorners(ByVal c1 As Color)
        DrawCorners(c1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal r1 As Rectangle)
        DrawCorners(c1, r1.X, r1.Y, r1.Width, r1.Height)
    End Sub
    Protected Sub DrawCorners(ByVal c1 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        If _NoRounding Then Return

        If _Transparent Then
            B.SetPixel(x, y, c1)
            B.SetPixel(x + (width - 1), y, c1)
            B.SetPixel(x, y + (height - 1), c1)
            B.SetPixel(x + (width - 1), y + (height - 1), c1)
        Else
            DrawCornersBrush = New SolidBrush(c1)
            G.FillRectangle(DrawCornersBrush, x, y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y, 1, 1)
            G.FillRectangle(DrawCornersBrush, x, y + (height - 1), 1, 1)
            G.FillRectangle(DrawCornersBrush, x + (width - 1), y + (height - 1), 1, 1)
        End If
    End Sub

#End Region

#Region " DrawBorders "

    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal offset As Integer)
        DrawBorders(p1, 0, 0, Width, Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle, ByVal offset As Integer)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height, offset)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal offset As Integer)
        DrawBorders(p1, x + offset, y + offset, width - (offset * 2), height - (offset * 2))
    End Sub

    Protected Sub DrawBorders(ByVal p1 As Pen)
        DrawBorders(p1, 0, 0, Width, Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal r As Rectangle)
        DrawBorders(p1, r.X, r.Y, r.Width, r.Height)
    End Sub
    Protected Sub DrawBorders(ByVal p1 As Pen, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        G.DrawRectangle(p1, x, y, width - 1, height - 1)
    End Sub

#End Region

#Region " DrawText "

    Private DrawTextPoint As Point
    Private DrawTextSize As Size

    Protected Sub DrawText(ByVal b1 As Brush, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawText(b1, Text, a, x, y)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal text As String, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If text.Length = 0 Then Return

        DrawTextSize = Measure(text)
        DrawTextPoint = Center(DrawTextSize)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawString(text, Font, b1, x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Center
                G.DrawString(text, Font, b1, DrawTextPoint.X + x, DrawTextPoint.Y + y)
            Case HorizontalAlignment.Right
                G.DrawString(text, Font, b1, Width - DrawTextSize.Width - x, DrawTextPoint.Y + y)
        End Select
    End Sub

    Protected Sub DrawText(ByVal b1 As Brush, ByVal p1 As Point)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, p1)
    End Sub
    Protected Sub DrawText(ByVal b1 As Brush, ByVal x As Integer, ByVal y As Integer)
        If Text.Length = 0 Then Return
        G.DrawString(Text, Font, b1, x, y)
    End Sub

#End Region

#Region " DrawImage "

    Private DrawImagePoint As Point

    Protected Sub DrawImage(ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, a, x, y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal a As HorizontalAlignment, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        DrawImagePoint = Center(image.Size)

        Select Case a
            Case HorizontalAlignment.Left
                G.DrawImage(image, x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Center
                G.DrawImage(image, DrawImagePoint.X + x, DrawImagePoint.Y + y, image.Width, image.Height)
            Case HorizontalAlignment.Right
                G.DrawImage(image, Width - image.Width - x, DrawImagePoint.Y + y, image.Width, image.Height)
        End Select
    End Sub

    Protected Sub DrawImage(ByVal p1 As Point)
        DrawImage(_Image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal x As Integer, ByVal y As Integer)
        DrawImage(_Image, x, y)
    End Sub

    Protected Sub DrawImage(ByVal image As Image, ByVal p1 As Point)
        DrawImage(image, p1.X, p1.Y)
    End Sub
    Protected Sub DrawImage(ByVal image As Image, ByVal x As Integer, ByVal y As Integer)
        If image Is Nothing Then Return
        G.DrawImage(image, x, y, image.Width, image.Height)
    End Sub

#End Region

#Region " DrawGradient "

    Private DrawGradientBrush As LinearGradientBrush
    Private DrawGradientRectangle As Rectangle

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(blend, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, 90.0F)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, Color.Empty, Color.Empty, angle)
        DrawGradientBrush.InterpolationColors = blend
        G.FillRectangle(DrawGradientBrush, r)
    End Sub


    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawGradientRectangle = New Rectangle(x, y, width, height)
        DrawGradient(c1, c2, DrawGradientRectangle, angle)
    End Sub

    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub
    Protected Sub DrawGradient(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawGradientBrush = New LinearGradientBrush(r, c1, c2, angle)
        G.FillRectangle(DrawGradientBrush, r)
    End Sub

#End Region

#Region " DrawRadial "

    Private DrawRadialPath As GraphicsPath
    Private DrawRadialBrush1 As PathGradientBrush
    Private DrawRadialBrush2 As LinearGradientBrush
    Private DrawRadialRectangle As Rectangle

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, width \ 2, height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal center As Point)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(blend, DrawRadialRectangle, cx, cy)
    End Sub

    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle)
        DrawRadial(blend, r, r.Width \ 2, r.Height \ 2)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal center As Point)
        DrawRadial(blend, r, center.X, center.Y)
    End Sub
    Sub DrawRadial(ByVal blend As ColorBlend, ByVal r As Rectangle, ByVal cx As Integer, ByVal cy As Integer)
        DrawRadialPath.Reset()
        DrawRadialPath.AddEllipse(r.X, r.Y, r.Width - 1, r.Height - 1)

        DrawRadialBrush1 = New PathGradientBrush(DrawRadialPath)
        DrawRadialBrush1.CenterPoint = New Point(r.X + cx, r.Y + cy)
        DrawRadialBrush1.InterpolationColors = blend

        If G.SmoothingMode = SmoothingMode.AntiAlias Then
            G.FillEllipse(DrawRadialBrush1, r.X + 1, r.Y + 1, r.Width - 3, r.Height - 3)
        Else
            G.FillEllipse(DrawRadialBrush1, r)
        End If
    End Sub


    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawRadialRectangle)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal angle As Single)
        DrawRadialRectangle = New Rectangle(x, y, width, height)
        DrawRadial(c1, c2, DrawRadialRectangle, angle)
    End Sub

    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, 90.0F)
        G.FillEllipse(DrawRadialBrush2, r)
    End Sub
    Protected Sub DrawRadial(ByVal c1 As Color, ByVal c2 As Color, ByVal r As Rectangle, ByVal angle As Single)
        DrawRadialBrush2 = New LinearGradientBrush(r, c1, c2, angle)
        G.FillEllipse(DrawRadialBrush2, r)
    End Sub

#End Region

#Region " CreateRound "

    Private CreateRoundPath As GraphicsPath
    Private CreateRoundRectangle As Rectangle

    Function CreateRound(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer, ByVal slope As Integer) As GraphicsPath
        CreateRoundRectangle = New Rectangle(x, y, width, height)
        Return CreateRound(CreateRoundRectangle, slope)
    End Function

    Function CreateRound(ByVal r As Rectangle, ByVal slope As Integer) As GraphicsPath
        CreateRoundPath = New GraphicsPath(FillMode.Winding)
        CreateRoundPath.AddArc(r.X, r.Y, slope, slope, 180.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Y, slope, slope, 270.0F, 90.0F)
        CreateRoundPath.AddArc(r.Right - slope, r.Bottom - slope, slope, slope, 0.0F, 90.0F)
        CreateRoundPath.AddArc(r.X, r.Bottom - slope, slope, slope, 90.0F, 90.0F)
        CreateRoundPath.CloseFigure()
        Return CreateRoundPath
    End Function

#End Region

End Class

Module ThemeShare

#Region " Animation "

    Private Frames As Integer
    Private Invalidate As Boolean
    Public ThemeTimer As New PrecisionTimer

    Private Const FPS As Integer = 50 '1000 / 50 = 20 FPS
    Private Const Rate As Integer = 10

    Public Delegate Sub AnimationDelegate(ByVal invalidate As Boolean)

    Private Callbacks As New List(Of AnimationDelegate)

    Private Sub HandleCallbacks(ByVal state As IntPtr, ByVal reserve As Boolean)
        Invalidate = (Frames >= FPS)
        If Invalidate Then Frames = 0

        SyncLock Callbacks
            For I As Integer = 0 To Callbacks.Count - 1
                Callbacks(I).Invoke(Invalidate)
            Next
        End SyncLock

        Frames += Rate
    End Sub

    Private Sub InvalidateThemeTimer()
        If Callbacks.Count = 0 Then
            ThemeTimer.Delete()
        Else
            ThemeTimer.Create(0, Rate, AddressOf HandleCallbacks)
        End If
    End Sub

    Sub AddAnimationCallback(ByVal callback As AnimationDelegate)
        SyncLock Callbacks
            If Callbacks.Contains(callback) Then Return

            Callbacks.Add(callback)
            InvalidateThemeTimer()
        End SyncLock
    End Sub

    Sub RemoveAnimationCallback(ByVal callback As AnimationDelegate)
        SyncLock Callbacks
            If Not Callbacks.Contains(callback) Then Return

            Callbacks.Remove(callback)
            InvalidateThemeTimer()
        End SyncLock
    End Sub

#End Region

End Module

Enum MouseState As Byte
    None = 0
    Over = 1
    Down = 2
    Block = 3
End Enum

Structure Bloom

    Public _Name As String
    ReadOnly Property Name() As String
        Get
            Return _Name
        End Get
    End Property

    Private _Value As Color
    Property Value() As Color
        Get
            Return _Value
        End Get
        Set(ByVal value As Color)
            _Value = value
        End Set
    End Property

    Property ValueHex() As String
        Get
            Return String.Concat("#",
            _Value.R.ToString("X2", Nothing),
            _Value.G.ToString("X2", Nothing),
            _Value.B.ToString("X2", Nothing))
        End Get
        Set(ByVal value As String)
            Try
                _Value = ColorTranslator.FromHtml(value)
            Catch
                Return
            End Try
        End Set
    End Property


    Sub New(ByVal name As String, ByVal value As Color)
        _Name = name
        _Value = value
    End Sub
End Structure

Class PrecisionTimer
    Implements IDisposable

    Private _Enabled As Boolean
    ReadOnly Property Enabled() As Boolean
        Get
            Return _Enabled
        End Get
    End Property

    Private Handle As IntPtr
    Private TimerCallback As TimerDelegate

    <DllImport("kernel32.dll", EntryPoint:="CreateTimerQueueTimer")>
    Private Shared Function CreateTimerQueueTimer(
    ByRef handle As IntPtr,
    ByVal queue As IntPtr,
    ByVal callback As TimerDelegate,
    ByVal state As IntPtr,
    ByVal dueTime As UInteger,
    ByVal period As UInteger,
    ByVal flags As UInteger) As Boolean
    End Function

    <DllImport("kernel32.dll", EntryPoint:="DeleteTimerQueueTimer")>
    Private Shared Function DeleteTimerQueueTimer(
    ByVal queue As IntPtr,
    ByVal handle As IntPtr,
    ByVal callback As IntPtr) As Boolean
    End Function

    Delegate Sub TimerDelegate(ByVal r1 As IntPtr, ByVal r2 As Boolean)

    Sub Create(ByVal dueTime As UInteger, ByVal period As UInteger, ByVal callback As TimerDelegate)
        If _Enabled Then Return

        TimerCallback = callback
        Dim Success As Boolean = CreateTimerQueueTimer(Handle, IntPtr.Zero, TimerCallback, IntPtr.Zero, dueTime, period, 0)

        If Not Success Then ThrowNewException("CreateTimerQueueTimer")
        _Enabled = Success
    End Sub

    Sub Delete()
        If Not _Enabled Then Return
        Dim Success As Boolean = DeleteTimerQueueTimer(IntPtr.Zero, Handle, IntPtr.Zero)

        If Not Success AndAlso Not Marshal.GetLastWin32Error = 997 Then
            ThrowNewException("DeleteTimerQueueTimer")
        End If

        _Enabled = Not Success
    End Sub

    Private Sub ThrowNewException(ByVal name As String)
        Throw New Exception(String.Format("{0} failed. Win32Error: {1}", name, Marshal.GetLastWin32Error))
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        Delete()
    End Sub
End Class

Class RTheme
    Inherits ThemeContainer154

    Sub New()
        TransparencyKey = Color.Fuchsia
        BackColor = Color.FromArgb(245, 245, 245)
        Font = New Font("Segoe UI", 9)
        SetColor("border", Color.FromArgb(39, 38, 38))
        SetColor("Text", Color.White)
    End Sub

    Dim Border As Color
    Dim TextBrush As Brush
    Protected Overrides Sub ColorHook()
        Border = GetColor("border")
        TextBrush = GetBrush("Text")
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(Border)
        ' Dim HB As New HatchBrush(HatchStyle.Percent80, Color.FromArgb(45, Color.FromArgb(39, 38, 38)), Color.Transparent)
        G.FillRectangle(New SolidBrush(BackColor), New Rectangle(3, 26, Width - 6, Height - 29))
        '.FillRectangle(HB, New Rectangle(6, 26, Width - 12, Height - 33))
        G.DrawString(FindForm.Text, Font, TextBrush, New Point(27, 3))
        G.DrawIcon(FindForm.Icon, New Rectangle(10, 3, 16, 16))
        DrawCorners(Color.Transparent)
    End Sub

End Class

Class RButtonRed
    Inherits ThemeControl154

    Dim ButtonColor As Color
    Dim TextColor As Brush
    Dim Border As Pen

    Sub New()
        SetColor("Button", Color.FromArgb(231, 76, 60))
        SetColor("Text", Color.White)
        SetColor("BorderColor", Color.White)
    End Sub

    Protected Overrides Sub ColorHook()
        ButtonColor = GetColor("Button")
        TextColor = GetBrush("Text")
        Border = GetPen("BorderColor")
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(ButtonColor)
        Select Case State
            Case MouseState.None
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
            Case MouseState.Over
                G.FillRectangle(New SolidBrush(Color.FromArgb(50, Color.White)), New Rectangle(0, 0, Width - 1, Height - 1))
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
            Case MouseState.Down
                G.FillRectangle(New SolidBrush(Color.FromArgb(50, Color.Silver)), New Rectangle(0, 0, Width - 1, Height - 1))
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
        End Select
    End Sub
End Class

Class RButtonBlack
    Inherits ThemeControl154

    Dim ButtonColor As Color
    Dim TextColor As Brush
    Dim Border As Pen

    Sub New()
        SetColor("Button", Color.FromArgb(39, 38, 38))
        SetColor("Text", Color.White)
        SetColor("BorderColor", Color.White)
    End Sub

    Protected Overrides Sub ColorHook()
        ButtonColor = GetColor("Button")
        TextColor = GetBrush("Text")
        Border = GetPen("BorderColor")
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(ButtonColor)
        Select Case State
            Case MouseState.None
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
            Case MouseState.Over
                G.FillRectangle(New SolidBrush(Color.FromArgb(50, Color.White)), New Rectangle(0, 0, Width - 1, Height - 1))
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
            Case MouseState.Down
                G.FillRectangle(New SolidBrush(Color.FromArgb(50, Color.Silver)), New Rectangle(0, 0, Width - 1, Height - 1))
                G.DrawRectangle(Border, New Rectangle(0, 0, Width - 1, Height - 1))
                DrawText(TextColor, HorizontalAlignment.Center, 0, 0)
                DrawCorners(Color.White)
        End Select
    End Sub
End Class

<DefaultEvent("CheckedChanged")>
Class RRadioButton
    Inherits ThemeControl154

    Sub New()
        LockHeight = 17
        SetColor("Text", Color.FromArgb(39, 38, 38))
        SetColor("Gradient 1", 236, 240, 241)
        SetColor("Gradient 2", 236, 240, 241)
        SetColor("Glow", 236, 240, 241)
        SetColor("Edges", 170, 170, 170)
        SetColor("Backcolor", Color.FromArgb(236, 240, 241))
        SetColor("Bullet", Color.FromArgb(231, 76, 60))
        Width = 180
    End Sub

    Private X As Integer
    Private TextColor, G1, G2, Glow, Edge, BG As Color

    Protected Overrides Sub ColorHook()
        TextColor = GetColor("Text")
        G1 = GetColor("Gradient 1")
        G2 = GetColor("Gradient 2")
        Glow = GetColor("Glow")
        Edge = GetColor("Edges")
        BG = GetColor("Backcolor")
    End Sub

    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(BG)
        '   G.SmoothingMode = SmoothingMode.HighQuality
        If _Checked Then
            Dim LGB As New LinearGradientBrush(New Rectangle(New Point(0, 0), New Size(14, 14)), G1, G2, 90.0F)
            G.FillEllipse(LGB, New Rectangle(New Point(0, 0), New Size(14, 14)))
            G.FillEllipse(New SolidBrush(Glow), New Rectangle(New Point(0, 0), New Size(14, 7)))
        Else
            Dim LGB As New LinearGradientBrush(New Rectangle(New Point(0, 0), New Size(14, 16)), G1, G2, 90.0F)
            G.FillEllipse(LGB, New Rectangle(New Point(0, 0), New Size(14, 14)))
            G.FillEllipse(New SolidBrush(Glow), New Rectangle(New Point(0, 0), New Size(14, 7)))
        End If

        If State = MouseState.Over And X < 15 Then
            Dim SB As New SolidBrush(Color.FromArgb(70, Color.White))
            G.FillEllipse(SB, New Rectangle(New Point(0, 0), New Size(14, 14)))
        ElseIf State = MouseState.Down And X < 15 Then
            Dim SB As New SolidBrush(Color.FromArgb(10, Color.Black))
            G.FillEllipse(SB, New Rectangle(New Point(0, 0), New Size(14, 14)))
        End If

        'Dim HB As New HatchBrush(HatchStyle.LightDownwardDiagonal, Color.FromArgb(7, Color.Black), Color.Transparent)
        'G.FillEllipse(HB, New Rectangle(New Point(0, 0), New Size(14, 14)))
        G.DrawEllipse(New Pen(Edge), New Rectangle(New Point(0, 0), New Size(14, 14)))

        If _Checked Then G.FillEllipse(GetBrush("Bullet"), New Rectangle(New Point(4, 4), New Size(6, 6)))
        DrawText(New SolidBrush(TextColor), HorizontalAlignment.Left, 19, -1)
    End Sub

    Private _Field As Integer = 16
    Property Field() As Integer
        Get
            Return _Field
        End Get
        Set(ByVal value As Integer)
            If value < 4 Then Return
            _Field = value
            LockHeight = value
            Invalidate()
        End Set
    End Property

    Private _Checked As Boolean
    Property Checked() As Boolean
        Get
            Return _Checked
        End Get
        Set(ByVal value As Boolean)
            _Checked = value
            InvalidateControls()
            RaiseEvent CheckedChanged(Me)
            Invalidate()
        End Set
    End Property

    Protected Overrides Sub OnMouseDown(ByVal e As System.Windows.Forms.MouseEventArgs)
        If Not _Checked Then Checked = True
        MyBase.OnMouseDown(e)
    End Sub

    Event CheckedChanged(ByVal sender As Object)

    Protected Overrides Sub OnCreation()
        InvalidateControls()
    End Sub

    Private Sub InvalidateControls()
        If Not IsHandleCreated OrElse Not _Checked Then Return

        For Each C As Control In Parent.Controls
            If C IsNot Me AndAlso TypeOf C Is RRadioButton Then
                DirectCast(C, RRadioButton).Checked = False
            End If
        Next
    End Sub

End Class

<DefaultEvent("CheckedChanged")>
Class RCheckbox
    Inherits ThemeControl154

    Sub New()
        LockHeight = 17
        SetColor("Text", Color.FromArgb(39, 38, 38))
        SetColor("Gradient 1", 230, 230, 230)
        SetColor("Gradient 2", 210, 210, 210)
        SetColor("Glow", 230, 230, 230)
        SetColor("Edges", 170, 170, 170)
        SetColor("Backcolor", 245, 245, 245)
        SetColor("BK", Color.FromArgb(245, 245, 245))
        Width = 160
    End Sub

    Private X As Integer
    Private TextColor, G1, G2, Glow, Edge, BG As Color
    Dim BK As Brush

    Protected Overrides Sub ColorHook()
        TextColor = GetColor("Text")
        G1 = GetColor("Gradient 1")
        G2 = GetColor("Gradient 2")
        Glow = GetColor("Glow")
        Edge = GetColor("Edges")
        BG = GetColor("Backcolor")
        BK = GetBrush("BK")
    End Sub

    Protected Overrides Sub OnMouseMove(e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        X = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(BG)
        If _Checked Then
            Dim LGB As New LinearGradientBrush(New Rectangle(New Point(0, 0), New Size(14, 14)), G1, G2, 90.0F)
            G.FillRectangle(LGB, New Rectangle(New Point(0, 0), New Size(14, 14)))
            G.FillRectangle(New SolidBrush(Glow), New Rectangle(New Point(0, 0), New Size(14, 7)))
        Else
            Dim LGB As New LinearGradientBrush(New Rectangle(New Point(0, 0), New Size(14, 16)), G1, G2, 90.0F)
            G.FillRectangle(LGB, New Rectangle(New Point(0, 0), New Size(14, 14)))
            G.FillRectangle(New SolidBrush(Glow), New Rectangle(New Point(0, 0), New Size(14, 7)))
        End If

        If State = MouseState.Over And X < 15 Then
            Dim SB As New SolidBrush(Color.FromArgb(70, Color.White))
            G.FillRectangle(SB, New Rectangle(New Point(0, 0), New Size(14, 14)))
        ElseIf State = MouseState.Down And X < 15 Then
            Dim SB As New SolidBrush(Color.FromArgb(10, Color.Black))
            G.FillRectangle(SB, New Rectangle(New Point(0, 0), New Size(14, 14)))
        End If


        G.FillRectangle(BK, New Rectangle(New Point(0, 0), New Size(14, 14)))
        G.DrawRectangle(New Pen(Edge), New Rectangle(New Point(0, 0), New Size(14, 14)))

        If _Checked Then G.DrawString("a", New Font("Marlett", 12), Brushes.OrangeRed, New Point(-3, -1))
        DrawText(New SolidBrush(TextColor), HorizontalAlignment.Left, 19, -1)
    End Sub

    Private _Checked As Boolean
    Property Checked() As Boolean
        Get
            Return _Checked
        End Get
        Set(ByVal value As Boolean)
            _Checked = value
            Invalidate()
        End Set
    End Property

    Protected Overrides Sub OnMouseDown(ByVal e As System.Windows.Forms.MouseEventArgs)
        _Checked = Not _Checked
        RaiseEvent CheckedChanged(Me)
        MyBase.OnMouseDown(e)
    End Sub

    Event CheckedChanged(ByVal sender As Object)

End Class

Class RGroupBox
    Inherits ThemeContainer154

    Sub New()
        ControlMode = True
        SetColor("Border", Color.FromArgb(39, 38, 38))
        SetColor("Header", Color.FromArgb(231, 76, 60))
        SetColor("TextColor", Color.FromArgb(236, 240, 241))
    End Sub

    Dim Border As Pen
    Dim HeaderColor, TextColor As Brush

    Protected Overrides Sub ColorHook()
        Border = GetPen("Border")
        HeaderColor = GetBrush("Header")
        TextColor = GetBrush("TextColor")
    End Sub

    Protected Overrides Sub PaintHook()
        G.Clear(BackColor)
        Dim HB As New HatchBrush(HatchStyle.Percent80, Color.FromArgb(0, Color.FromArgb(39, 38, 38)), Color.Transparent)
        G.FillRectangle(HeaderColor, New Rectangle(0, 0, Width - 2, 25))
        G.FillRectangle(HB, New Rectangle(0, 0, Width - 2, Height - 1))

        G.DrawRectangle(Border, New Rectangle(0, 0, Width - 2, 25))
        G.DrawRectangle(Border, New Rectangle(0, 0, Width - 2, Height - 1))
        G.DrawString(Text, Font, TextColor, New Point(5, 5))
    End Sub
End Class

Class RListview
    Inherits ListView

    <DllImport("uxtheme", CharSet:=CharSet.Unicode)>
    Public Shared Function SetWindowTheme(ByVal hWnd As IntPtr, ByVal textSubAppName As String, ByVal textSubIdList As String) As Integer
    End Function

    Public Sub New()
        Me.SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
        Me.DoubleBuffered = True
        BackColor = Color.FromArgb(236, 240, 241)
        HeaderStyle = ColumnHeaderStyle.Nonclickable
        BorderStyle = Windows.Forms.BorderStyle.FixedSingle ' Add the control to iTalk_Panel then full-dock it
    End Sub

    Protected Overrides Sub OnHandleCreated(ByVal e As EventArgs)
        RListview.SetWindowTheme(Me.Handle, "explorer", Nothing)
        MyBase.OnHandleCreated(e)
    End Sub
End Class

Class RTabControl
    Inherits TabControl
    Sub New()
        SetStyle(ControlStyles.AllPaintingInWmPaint Or
                     ControlStyles.UserPaint Or
                     ControlStyles.ResizeRedraw Or
                     ControlStyles.DoubleBuffer, True)

        DoubleBuffered = True
        SizeMode = TabSizeMode.Fixed
        ItemSize = New Size(44, 135)
        DrawMode = TabDrawMode.OwnerDrawFixed

        For Each Page As TabPage In Me.TabPages
            Page.BackColor = Color.FromArgb(236, 240, 241)
        Next
    End Sub

    Protected Overrides Sub OnControlAdded(ByVal e As ControlEventArgs)
        MyBase.OnControlAdded(e)
        If TypeOf e.Control Is TabPage Then
            For Each i As TabPage In Me.Controls
                i = New TabPage
            Next
            e.Control.BackColor = Color.FromArgb(236, 240, 241)
        End If
    End Sub

    Protected Overrides Sub CreateHandle()
        MyBase.CreateHandle()

        MyBase.DoubleBuffered = True
        SizeMode = TabSizeMode.Fixed
        Appearance = TabAppearance.Normal
        Alignment = TabAlignment.Left
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)
        MyBase.OnPaint(e)
        Dim B As New Bitmap(Width, Height)
        Dim G As Graphics = Graphics.FromImage(B)

        With G

            .Clear(Color.FromArgb(236, 240, 241))
            .SmoothingMode = SmoothingMode.HighSpeed
            .CompositingQuality = Drawing2D.CompositingQuality.HighSpeed
            .CompositingMode = Drawing2D.CompositingMode.SourceOver

            ' Draw tab selector background
            .FillRectangle(New SolidBrush(Color.FromArgb(39, 38, 38)), New Rectangle(-5, 0, ItemSize.Height + 4, Height))
            ' Draw vertical line at the end of the tab selector rectangle
            .DrawLine(New Pen(Color.FromArgb(39, 38, 38)), ItemSize.Height - 1, 0, ItemSize.Height - 1, Height)

            For TabIndex As Integer = 0 To TabCount - 1
                If TabIndex = SelectedIndex Then
                    Dim TabRect As Rectangle = New Rectangle(New Point(GetTabRect(TabIndex).Location.X - 2, GetTabRect(TabIndex).Location.Y - 2), New Size(GetTabRect(TabIndex).Width + 3, GetTabRect(TabIndex).Height - 8))

                    ' Draw background of the selected tab
                    .FillRectangle(New SolidBrush(Color.FromArgb(39, 38, 38)), TabRect.X, TabRect.Y, TabRect.Width - 4, TabRect.Height + 3)
                    ' Draw a tab highlighter on the background of the selected tab
                    Dim TabHighlighter As Rectangle = New Rectangle(New Point(GetTabRect(TabIndex).X - 2, GetTabRect(TabIndex).Location.Y - IIf(TabIndex = 0, 1, 1)), New Size(4, GetTabRect(TabIndex).Height - 7))
                    .FillRectangle(New SolidBrush(Color.FromArgb(236, 240, 241)), TabHighlighter)
                    ' Draw tab text
                    .DrawString(TabPages(TabIndex).Text, New Font(Font.FontFamily, Font.Size, FontStyle.Regular), New SolidBrush(Color.FromArgb(231, 76, 60)), New Rectangle(TabRect.Left + 40, TabRect.Top + 12, TabRect.Width - 40, TabRect.Height), New StringFormat With {.Alignment = StringAlignment.Near})

                    If Me.ImageList IsNot Nothing Then
                        Dim Index As Integer = TabPages(TabIndex).ImageIndex
                        If Not Index = -1 Then
                            .DrawImage(Me.ImageList.Images.Item(TabPages(TabIndex).ImageIndex), TabRect.X + 9, TabRect.Y + 6, 24, 24)
                        End If
                    End If

                Else

                    Dim TabRect As Rectangle = New Rectangle(New Point(GetTabRect(TabIndex).Location.X - 2, GetTabRect(TabIndex).Location.Y - 2), New Size(GetTabRect(TabIndex).Width + 3, GetTabRect(TabIndex).Height - 8))
                    ' Draw tab text
                    .DrawString(TabPages(TabIndex).Text, New Font(Font.FontFamily, Font.Size, FontStyle.Regular), New SolidBrush(Color.FromArgb(255, 255, 255)), New Rectangle(TabRect.Left + 40, TabRect.Top + 12, TabRect.Width - 40, TabRect.Height), New StringFormat With {.Alignment = StringAlignment.Near})

                    If Me.ImageList IsNot Nothing Then
                        Dim Index As Integer = TabPages(TabIndex).ImageIndex
                        If Not Index = -1 Then
                            .DrawImage(Me.ImageList.Images.Item(TabPages(TabIndex).ImageIndex), TabRect.X + 9, TabRect.Y + 6, 24, 24)
                        End If
                    End If

                End If
            Next
            e.Graphics.SmoothingMode = SmoothingMode.HighQuality
            e.Graphics.InterpolationMode = InterpolationMode.HighQualityBicubic
            e.Graphics.CompositingQuality = CompositingQuality.HighQuality
            e.Graphics.DrawImage(B.Clone, 0, 0)
            G.Dispose()
            B.Dispose()
        End With
    End Sub
End Class

Public Class RControlBox
    Inherits Control


    Enum MouseState As Byte
        None = 0
        Over = 1
        Down = 2
        Block = 3
    End Enum


    Dim State As MouseState = MouseState.None
    Dim i As Integer
    Dim CloseRect As New Rectangle(28, 0, 47, 18)
    Dim MinimizeRect As New Rectangle(0, 0, 28, 18)

    Protected Overrides Sub OnMouseClick(e As MouseEventArgs)
        MyBase.OnMouseClick(e)

        If i > 0 And i < 28 Then
            FindForm.WindowState = FormWindowState.Minimized
        ElseIf i > 30 And i < 75 Then
            FindForm.Close()
        End If

        State = MouseState.Down
    End Sub

    Protected Overrides Sub OnMouseEnter(ByVal e As System.EventArgs)
        MyBase.OnMouseEnter(e)
        State = MouseState.Over
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As System.EventArgs)
        MyBase.OnMouseLeave(e)
        State = MouseState.None
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseUp(e)
        State = MouseState.Over
        Invalidate()
    End Sub

    Protected Overrides Sub OnMouseMove(ByVal e As System.Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        i = e.Location.X
        Invalidate()
    End Sub

    Protected Overrides Sub OnResize(e As EventArgs)
        MyBase.OnResize(e)
        Width = 50
        Height = 19
    End Sub

    Sub New()
        SetStyle(ControlStyles.SupportsTransparentBackColor Or ControlStyles.UserPaint, True)
        BackColor = Color.Transparent
        DoubleBuffered = True
        Anchor = AnchorStyles.Top Or AnchorStyles.Right
    End Sub
    Protected Overrides Sub OnCreateControl()
        MyBase.OnCreateControl()
        '   Location = New Point(FindForm.Width - 81, -1) ' Auto-decide control location on the theme container
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        MyBase.OnPaint(e)

        Dim B As New Bitmap(Width, Height)
        Dim G As Graphics = Graphics.FromImage(B)
        Dim GP_MinimizeRect As New GraphicsPath
        Dim GP_CloseRect As New GraphicsPath

        GP_MinimizeRect.AddRectangle(MinimizeRect)
        GP_CloseRect.AddRectangle(CloseRect)
        G.Clear(BackColor)

        Select Case State
            Case MouseState.None
NonePoint:      Dim MinimizeGradient As New LinearGradientBrush(MinimizeRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                G.FillPath(MinimizeGradient, GP_MinimizeRect)
                G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_MinimizeRect)
                G.DrawString("0", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(255, 255, 255)), MinimizeRect.Width - 20, MinimizeRect.Height - 11)

                Dim CloseGradient As New LinearGradientBrush(CloseRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                G.FillPath(CloseGradient, GP_CloseRect)
                G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_CloseRect)
                G.DrawString("r", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(255, 255, 255)), CloseRect.Width - 16, CloseRect.Height - 11)
            Case MouseState.Over
                If i > 0 And i < 28 Then
                    Dim MinimizeGradient As New LinearGradientBrush(MinimizeRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                    G.FillPath(MinimizeGradient, GP_MinimizeRect)
                    G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_MinimizeRect)
                    G.DrawString("0", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(231, 76, 60)), MinimizeRect.Width - 20, MinimizeRect.Height - 11)

                    Dim CloseGradient As New LinearGradientBrush(CloseRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                    G.FillPath(CloseGradient, GP_CloseRect)
                    G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_CloseRect)
                    G.DrawString("r", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(255, 255, 255)), CloseRect.Width - 16, CloseRect.Height - 11)
                ElseIf i > 30 And i < 75 Then
                    Dim CloseGradient As New LinearGradientBrush(CloseRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                    G.FillPath(CloseGradient, GP_CloseRect)
                    G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_CloseRect)
                    G.DrawString("r", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(231, 76, 60)), CloseRect.Width - 16, CloseRect.Height - 11)

                    Dim MinimizeGradient As New LinearGradientBrush(MinimizeRect, Color.FromArgb(39, 38, 38), Color.FromArgb(39, 38, 38), 90S)
                    G.FillPath(MinimizeGradient, RoundRectanglee.RoundRect(MinimizeRect, 1))
                    G.DrawPath(New Pen(Color.FromArgb(39, 38, 38)), GP_MinimizeRect)
                    G.DrawString("0", New Font("Marlett", 7, FontStyle.Regular), New SolidBrush(Color.FromArgb(255, 255, 255)), MinimizeRect.Width - 20, MinimizeRect.Height - 11)
                Else
                    GoTo NonePoint ' Return to [MouseState = None]
                End If
        End Select

        e.Graphics.DrawImage(B.Clone(), 0, 0)
        G.Dispose()
        GP_CloseRect.Dispose()
        GP_MinimizeRect.Dispose()
        B.Dispose()
    End Sub

End Class

Module RoundRectanglee
    Public Function RoundRect(ByVal Rectangle As Rectangle, ByVal Curve As Integer) As GraphicsPath
        Dim GP As GraphicsPath = New GraphicsPath()
        Dim EndArcWidth As Integer = Curve * 2
        GP.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, EndArcWidth, EndArcWidth), -180, 90)
        GP.AddArc(New Rectangle(Rectangle.Width - EndArcWidth + Rectangle.X, Rectangle.Y, EndArcWidth, EndArcWidth), -90, 90)
        GP.AddArc(New Rectangle(Rectangle.Width - EndArcWidth + Rectangle.X, Rectangle.Height - EndArcWidth + Rectangle.Y, EndArcWidth, EndArcWidth), 0, 90)
        GP.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - EndArcWidth + Rectangle.Y, EndArcWidth, EndArcWidth), 90, 90)
        GP.AddLine(New Point(Rectangle.X, Rectangle.Height - EndArcWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        Return GP
    End Function
End Module

Module DrawHelpers

#Region "Functions"


    Public Function RoundRectangle(ByVal Rectangle As Rectangle, ByVal Curve As Integer) As GraphicsPath
        Dim P As GraphicsPath = New GraphicsPath()
        Dim ArcRectangleWidth As Integer = Curve * 2
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -180, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), -90, 90)
        P.AddArc(New Rectangle(Rectangle.Width - ArcRectangleWidth + Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 0, 90)
        P.AddArc(New Rectangle(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y, ArcRectangleWidth, ArcRectangleWidth), 90, 90)
        P.AddLine(New Point(Rectangle.X, Rectangle.Height - ArcRectangleWidth + Rectangle.Y), New Point(Rectangle.X, Curve + Rectangle.Y))
        Return P
    End Function

    Public Function RoundRect(x!, y!, w!, h!, Optional r! = 0.3, Optional TL As Boolean = True, Optional TR As Boolean = True, Optional BR As Boolean = True, Optional BL As Boolean = True) As GraphicsPath
        Dim d! = Math.Min(w, h) * r, xw = x + w, yh = y + h
        RoundRect = New GraphicsPath

        With RoundRect
            If TL Then .AddArc(x, y, d, d, 180, 90) Else .AddLine(x, y, x, y)
            If TR Then .AddArc(xw - d, y, d, d, 270, 90) Else .AddLine(xw, y, xw, y)
            If BR Then .AddArc(xw - d, yh - d, d, d, 0, 90) Else .AddLine(xw, yh, xw, yh)
            If BL Then .AddArc(x, yh - d, d, d, 90, 90) Else .AddLine(x, yh, x, yh)

            .CloseFigure()
        End With
    End Function

    Enum MouseState As Byte
        None = 0
        Over = 1
        Down = 2
        Block = 3
    End Enum

#End Region

End Module

Public Class RTrackBarRed
    Inherits Control

#Region "Declaration"
    Private _Maximum As Integer = 100
    Private _Value As Integer = 0
    Private CaptureMovement As Boolean = False
    Private Bar As Rectangle = New Rectangle(0, 10, Width - 21, Height - 21)
    Private Track As Size = New Size(15, 14)
    Private _TextColour As Color = Color.FromArgb(255, 255, 255)
    Private _BorderColour As Color = Color.FromArgb(39, 38, 38)
    Private _BarBaseColour As Color = Color.FromArgb(39, 38, 38)
    Private _StripColour As Color = Color.FromArgb(39, 38, 38)
    Private _StripAmountColour As Color = Color.FromArgb(231, 76, 60)
#End Region

#Region "Properties"

    <Category("Colours")>
    Public Property BorderColour As Color
        Get
            Return _BorderColour
        End Get
        Set(value As Color)
            _BorderColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property BarBaseColour As Color
        Get
            Return _BarBaseColour
        End Get
        Set(value As Color)
            _BarBaseColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property StripColour As Color
        Get
            Return _StripColour
        End Get
        Set(value As Color)
            _StripColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property TextColour As Color
        Get
            Return _TextColour
        End Get
        Set(value As Color)
            _TextColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property StripAmountColour As Color
        Get
            Return _StripAmountColour
        End Get
        Set(value As Color)
            _StripAmountColour = value
        End Set
    End Property

    Public Property Maximum() As Integer
        Get
            Return _Maximum
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then _Maximum = value
            If value < _Value Then _Value = value
            Invalidate()
        End Set
    End Property

    Event ValueChanged()

    Public Property Value() As Integer
        Get
            Return _Value
        End Get
        Set(ByVal value As Integer)
            Select Case value
                Case Is = _Value
                    Exit Property
                Case Is < 0
                    _Value = 0
                Case Is > _Maximum
                    _Value = _Maximum
                Case Else
                    _Value = value
            End Select
            Invalidate()
            RaiseEvent ValueChanged()
        End Set
    End Property

    Protected Overrides Sub OnHandleCreated(ByVal e As System.EventArgs)
        BackColor = Color.Transparent
        MyBase.OnHandleCreated(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseDown(e)
        Dim MovementPoint As New Rectangle(New Point(e.Location.X, e.Location.Y), New Size(1, 1))
        Dim Bar As New Rectangle(10, 10, Width - 21, Height - 21)
        If New Rectangle(New Point(Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2 - 1), 0), New Size(Track.Width, Height)).IntersectsWith(MovementPoint) Then
            CaptureMovement = True
        End If
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseUp(e)
        CaptureMovement = False
    End Sub

    Protected Overrides Sub OnMouseMove(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        If CaptureMovement Then
            Dim MovementPoint As New Point(e.X, e.Y)
            Dim Bar As New Rectangle(10, 10, Width - 21, Height - 21)
            Value = CInt(Maximum * ((MovementPoint.X - Bar.X) / Bar.Width))
        End If
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        MyBase.OnMouseLeave(e) : CaptureMovement = False
    End Sub

#End Region

#Region "Draw Control"

    Sub New()
        SetStyle(ControlStyles.OptimizedDoubleBuffer Or ControlStyles.AllPaintingInWmPaint Or ControlStyles.ResizeRedraw Or
                    ControlStyles.UserPaint Or ControlStyles.Selectable Or
                    ControlStyles.SupportsTransparentBackColor, True)
        DoubleBuffered = True
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        Dim B As New Bitmap(Width, Height)
        Dim G = Graphics.FromImage(B)
        With G
            .SmoothingMode = SmoothingMode.HighQuality
            .PixelOffsetMode = PixelOffsetMode.HighQuality
            .TextRenderingHint = TextRenderingHint.ClearTypeGridFit
            Bar = New Rectangle(13, 11, Width - 27, Height - 21)
            .Clear(Parent.FindForm.BackColor)
            .SmoothingMode = SmoothingMode.AntiAlias
            .TextRenderingHint = TextRenderingHint.AntiAliasGridFit
            .FillRectangle(New SolidBrush(_StripColour), New Rectangle(3, CInt((Height / 2) - 4), Width - 5, 8))
            .DrawRectangle(New Pen(_BorderColour, 2), New Rectangle(4, CInt((Height / 2) - 4), Width - 5, 8))
            .FillRectangle(New SolidBrush(_StripAmountColour), New Rectangle(4, CInt((Height / 2) - 4), CInt(Bar.Width * (Value / Maximum)) + CInt(Track.Width / 2), 8))
            .FillRectangle(New SolidBrush(_BarBaseColour), Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width, Track.Height)
            .DrawRectangle(New Pen(_BorderColour, 2), Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width, Track.Height)
            '   .DrawString(_Value, New Font("Segoe UI", 6.5, FontStyle.Regular), New SolidBrush(_TextColour), New Rectangle(Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width - 1, Track.Height), New StringFormat With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center})
        End With
        MyBase.OnPaint(e)
        G.Dispose()
        e.Graphics.InterpolationMode = InterpolationMode.HighQualityBicubic
        e.Graphics.DrawImageUnscaled(B, 0, 0)
        B.Dispose()
    End Sub

#End Region

End Class

Public Class RTrackBarWhite
    Inherits Control

#Region "Declaration"
    Private _Maximum As Integer = 100
    Private _Value As Integer = 0
    Private CaptureMovement As Boolean = False
    Private Bar As Rectangle = New Rectangle(0, 10, Width - 21, Height - 21)
    Private Track As Size = New Size(15, 14)
    Private _TextColour As Color = Color.FromArgb(255, 255, 255)
    Private _BorderColour As Color = Color.FromArgb(39, 38, 38)
    Private _BarBaseColour As Color = Color.FromArgb(39, 38, 38)
    Private _StripColour As Color = Color.FromArgb(39, 38, 38)
    Private _StripAmountColour As Color = Color.FromArgb(236, 240, 241)
#End Region

#Region "Properties"

    <Category("Colours")>
    Public Property BorderColour As Color
        Get
            Return _BorderColour
        End Get
        Set(value As Color)
            _BorderColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property BarBaseColour As Color
        Get
            Return _BarBaseColour
        End Get
        Set(value As Color)
            _BarBaseColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property StripColour As Color
        Get
            Return _StripColour
        End Get
        Set(value As Color)
            _StripColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property TextColour As Color
        Get
            Return _TextColour
        End Get
        Set(value As Color)
            _TextColour = value
        End Set
    End Property

    <Category("Colours")>
    Public Property StripAmountColour As Color
        Get
            Return _StripAmountColour
        End Get
        Set(value As Color)
            _StripAmountColour = value
        End Set
    End Property

    Public Property Maximum() As Integer
        Get
            Return _Maximum
        End Get
        Set(ByVal value As Integer)
            If value > 0 Then _Maximum = value
            If value < _Value Then _Value = value
            Invalidate()
        End Set
    End Property

    Event ValueChanged()

    Public Property Value() As Integer
        Get
            Return _Value
        End Get
        Set(ByVal value As Integer)
            Select Case value
                Case Is = _Value
                    Exit Property
                Case Is < 0
                    _Value = 0
                Case Is > _Maximum
                    _Value = _Maximum
                Case Else
                    _Value = value
            End Select
            Invalidate()
            RaiseEvent ValueChanged()
        End Set
    End Property

    Protected Overrides Sub OnHandleCreated(ByVal e As System.EventArgs)
        BackColor = Color.Transparent
        MyBase.OnHandleCreated(e)
    End Sub

    Protected Overrides Sub OnMouseDown(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseDown(e)
        Dim MovementPoint As New Rectangle(New Point(e.Location.X, e.Location.Y), New Size(1, 1))
        Dim Bar As New Rectangle(10, 10, Width - 21, Height - 21)
        If New Rectangle(New Point(Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2 - 1), 0), New Size(Track.Width, Height)).IntersectsWith(MovementPoint) Then
            CaptureMovement = True
        End If
    End Sub

    Protected Overrides Sub OnMouseUp(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseUp(e)
        CaptureMovement = False
    End Sub

    Protected Overrides Sub OnMouseMove(ByVal e As Windows.Forms.MouseEventArgs)
        MyBase.OnMouseMove(e)
        If CaptureMovement Then
            Dim MovementPoint As New Point(e.X, e.Y)
            Dim Bar As New Rectangle(10, 10, Width - 21, Height - 21)
            Value = CInt(Maximum * ((MovementPoint.X - Bar.X) / Bar.Width))
        End If
    End Sub

    Protected Overrides Sub OnMouseLeave(ByVal e As EventArgs)
        MyBase.OnMouseLeave(e) : CaptureMovement = False
    End Sub

#End Region

#Region "Draw Control"

    Sub New()
        SetStyle(ControlStyles.OptimizedDoubleBuffer Or ControlStyles.AllPaintingInWmPaint Or ControlStyles.ResizeRedraw Or
                    ControlStyles.UserPaint Or ControlStyles.Selectable Or
                    ControlStyles.SupportsTransparentBackColor, True)
        DoubleBuffered = True
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        Dim B As New Bitmap(Width, Height)
        Dim G = Graphics.FromImage(B)
        With G
            .SmoothingMode = SmoothingMode.HighQuality
            .PixelOffsetMode = PixelOffsetMode.HighQuality
            .TextRenderingHint = TextRenderingHint.ClearTypeGridFit
            Bar = New Rectangle(13, 11, Width - 27, Height - 21)
            .Clear(Parent.FindForm.BackColor)
            .SmoothingMode = SmoothingMode.AntiAlias
            .TextRenderingHint = TextRenderingHint.AntiAliasGridFit
            .FillRectangle(New SolidBrush(_StripColour), New Rectangle(3, CInt((Height / 2) - 4), Width - 5, 8))
            .DrawRectangle(New Pen(_BorderColour, 2), New Rectangle(4, CInt((Height / 2) - 4), Width - 5, 8))
            .FillRectangle(New SolidBrush(_StripAmountColour), New Rectangle(4, CInt((Height / 2) - 4), CInt(Bar.Width * (Value / Maximum)) + CInt(Track.Width / 2), 8))
            .FillRectangle(New SolidBrush(_BarBaseColour), Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width, Track.Height)
            .DrawRectangle(New Pen(_BorderColour, 2), Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width, Track.Height)
            '   .DrawString(_Value, New Font("Segoe UI", 6.5, FontStyle.Regular), New SolidBrush(_TextColour), New Rectangle(Bar.X + CInt(Bar.Width * (Value / Maximum)) - CInt(Track.Width / 2), Bar.Y + CInt((Bar.Height / 2)) - CInt(Track.Height / 2), Track.Width - 1, Track.Height), New StringFormat With {.Alignment = StringAlignment.Center, .LineAlignment = StringAlignment.Center})
        End With
        MyBase.OnPaint(e)
        G.Dispose()
        e.Graphics.InterpolationMode = InterpolationMode.HighQualityBicubic
        e.Graphics.DrawImageUnscaled(B, 0, 0)
        B.Dispose()
    End Sub

#End Region

End Class


Module Theme

    Public ReadOnly Property GlobalFont(B As FontStyle, S As Integer) As Font
        Get
            Return New Font("Segoe UI", S, B)
        End Get
    End Property

    Public Function GetCheckMark() As String
        Return "iVBORw0KGgoAAAANSUhEUgAAABMAAAAQCAYAAAD0xERiAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAEySURBVDhPY/hPRUBdw/79+/efVHz77bf/X37+wRAn2bDff/7+91l+83/YmtsYBpJs2ITjz/8rTbrwP2Dlrf9XXn5FkSPJsD13P/y3nHsVbNjyy28w5Ik27NWXX//TNt8DG1S19zFWNRiGvfzy8//ccy9RxEB4wvFnYIMMZl7+//brLwx5EEYx7MP33/9dF18Ha1py8RVcHBR7mlMvgsVXX8X0Hgwz/P379z8yLtz5AKxJdcpFcBj9+v3nf/CqW2Cx5E13UdSiYwzDvv36/d9/BUSzzvRL/0t2PQSzQd57+vEHilp0jGEYCJ9+8hnuGhiee+4Vhjp0jNUwEN566/1/m/mQZJC/48H/zz9+YVWHjHEaBsKgwAZ59eH771jl0TFew0D48osvWMWxYYKGEY///gcAqiuA6kEmfEMAAAAASUVORK5CYII="
    End Function

End Module

Module Helpers

    Public Enum MouseState As Byte
        None = 0
        Over = 1
        Down = 2
    End Enum

    Public Function FullRectangle(S As Size, Subtract As Boolean) As Rectangle
        If Subtract Then
            Return New Rectangle(0, 0, S.Width - 1, S.Height - 1)
        Else
            Return New Rectangle(0, 0, S.Width, S.Height)
        End If
    End Function

    Public Function GreyColor(G As UInteger) As Color
        Return Color.FromArgb(G, G, G)
    End Function

    Public Sub CenterString(G As Graphics, T As String, F As Font, C As Color, R As Rectangle)
        Dim TS As SizeF = G.MeasureString(T, F)

        Using B As New SolidBrush(C)
            G.DrawString(T, F, B, New Point(R.Width / 2 - (TS.Width / 2), R.Height / 2 - (TS.Height / 2)))
        End Using
    End Sub

    Public Sub FillRoundRect(G As Graphics, R As Rectangle, Curve As Integer, C As Color)

        Using B As New SolidBrush(C)
            G.FillPie(B, R.X, R.Y, Curve, Curve, 180, 90)
            G.FillPie(B, R.X + R.Width - Curve, R.Y, Curve, Curve, 270, 90)
            G.FillPie(B, R.X, R.Y + R.Height - Curve, Curve, Curve, 90, 90)
            G.FillPie(B, R.X + R.Width - Curve, R.Y + R.Height - Curve, Curve, Curve, 0, 90)
            G.FillRectangle(B, CInt(R.X + Curve / 2), R.Y, R.Width - Curve, CInt(Curve / 2))
            G.FillRectangle(B, R.X, CInt(R.Y + Curve / 2), R.Width, R.Height - Curve)
            G.FillRectangle(B, CInt(R.X + Curve / 2), CInt(R.Y + R.Height - Curve / 2), R.Width - Curve, CInt(Curve / 2))
        End Using

    End Sub

    Public Sub DrawRoundRect(G As Graphics, R As Rectangle, Curve As Integer, C As Color)

        Using P As New Pen(C)
            G.DrawArc(P, R.X, R.Y, Curve, Curve, 180, 90)
            G.DrawLine(P, CInt(R.X + Curve / 2), R.Y, CInt(R.X + R.Width - Curve / 2), R.Y)
            G.DrawArc(P, R.X + R.Width - Curve, R.Y, Curve, Curve, 270, 90)
            G.DrawLine(P, R.X, CInt(R.Y + Curve / 2), R.X, CInt(R.Y + R.Height - Curve / 2))
            G.DrawLine(P, CInt(R.X + R.Width), CInt(R.Y + Curve / 2), CInt(R.X + R.Width), CInt(R.Y + R.Height - Curve / 2))
            G.DrawLine(P, CInt(R.X + Curve / 2), CInt(R.Y + R.Height), CInt(R.X + R.Width - Curve / 2), CInt(R.Y + R.Height))
            G.DrawArc(P, R.X, R.Y + R.Height - Curve, Curve, Curve, 90, 90)
            G.DrawArc(P, R.X + R.Width - Curve, R.Y + R.Height - Curve, Curve, Curve, 0, 90)
        End Using

    End Sub

    Public Sub CenterStringTab(G As Graphics, text As String, font As Font, brush As Brush, rect As Rectangle, Optional shadow As Boolean = False, Optional yOffset As Integer = 0)

        Dim textSize As SizeF = G.MeasureString(text, font)
        Dim textX As Integer = rect.X + (rect.Width / 2) - (textSize.Width / 2)
        Dim textY As Integer = rect.Y + (rect.Height / 2) - (textSize.Height / 2) + yOffset

        If shadow Then G.DrawString(text, font, Brushes.Black, textX + 1, textY + 1)
        G.DrawString(text, font, brush, textX, textY + 1)

    End Sub

End Module

Class FirefoxTextbox
    Inherits Control

#Region " Private "
    Private WithEvents TB As New TextBox
    Private G As Graphics
    Private State As MouseState
    Private IsDown As Boolean

    Private _EnabledCalc As Boolean
    Private _allowpassword As Boolean = False
    Private _maxChars As Integer = 32767
    Private _textAlignment As HorizontalAlignment
    Private _multiLine As Boolean = False
    Private _readOnly As Boolean = False
#End Region

#Region " Properties "

    Public Shadows Property Enabled As Boolean
        Get
            Return EnabledCalc
        End Get
        Set(value As Boolean)
            TB.Enabled = value
            _EnabledCalc = value
            Invalidate()
        End Set
    End Property

    <DisplayName("Enabled")>
    Public Property EnabledCalc As Boolean
        Get
            Return _EnabledCalc
        End Get
        Set(value As Boolean)
            Enabled = value
            Invalidate()
        End Set
    End Property

    Public Shadows Property UseSystemPasswordChar() As Boolean
        Get
            Return _allowpassword
        End Get
        Set(ByVal value As Boolean)
            TB.UseSystemPasswordChar = UseSystemPasswordChar
            _allowpassword = value
            Invalidate()
        End Set
    End Property

    Public Shadows Property MaxLength() As Integer
        Get
            Return _maxChars
        End Get
        Set(ByVal value As Integer)
            _maxChars = value
            TB.MaxLength = MaxLength
            Invalidate()
        End Set
    End Property

    Public Shadows Property TextAlign() As HorizontalAlignment
        Get
            Return _textAlignment
        End Get
        Set(ByVal value As HorizontalAlignment)
            _textAlignment = value
            Invalidate()
        End Set
    End Property

    Public Shadows Property MultiLine() As Boolean
        Get
            Return _multiLine
        End Get
        Set(ByVal value As Boolean)
            _multiLine = value
            TB.Multiline = value
            OnResize(EventArgs.Empty)
            Invalidate()
        End Set
    End Property

    Public Shadows Property [ReadOnly]() As Boolean
        Get
            Return _readOnly
        End Get
        Set(ByVal value As Boolean)
            _readOnly = value
            If TB IsNot Nothing Then
                TB.ReadOnly = value
            End If
        End Set
    End Property

#End Region

#Region " Control "

    Protected Overrides Sub OnTextChanged(ByVal e As EventArgs)
        MyBase.OnTextChanged(e)
        Invalidate()
    End Sub

    Protected Overrides Sub OnBackColorChanged(ByVal e As EventArgs)
        MyBase.OnBackColorChanged(e)
        Invalidate()
    End Sub

    Protected Overrides Sub OnForeColorChanged(ByVal e As EventArgs)
        MyBase.OnForeColorChanged(e)
        TB.ForeColor = ForeColor
        Invalidate()
    End Sub

    Protected Overrides Sub OnFontChanged(ByVal e As EventArgs)
        MyBase.OnFontChanged(e)
        TB.Font = Font
    End Sub

    Protected Overrides Sub OnGotFocus(ByVal e As EventArgs)
        MyBase.OnGotFocus(e)
        TB.Focus()
    End Sub

    Private Sub TextChangeTb() Handles TB.TextChanged
        Text = TB.Text
    End Sub

    Private Sub TextChng() Handles MyBase.TextChanged
        TB.Text = Text
    End Sub

    Public Sub NewTextBox()
        With TB
            .Text = String.Empty
            .BackColor = Color.FromArgb(236, 240, 241)
            .ForeColor = Color.FromArgb(231, 76, 60)
            .TextAlign = HorizontalAlignment.Left
            .BorderStyle = BorderStyle.None
            .Location = New Point(3, 3)
            .Font = GlobalFont(FontStyle.Regular, 10)
            .Size = New Size(Width - 3, Height - 3)
            .UseSystemPasswordChar = UseSystemPasswordChar
        End With
    End Sub

    Sub New()
        MyBase.New()
        NewTextBox()
        Controls.Add(TB)
        SetStyle(ControlStyles.UserPaint Or ControlStyles.SupportsTransparentBackColor, True)
        DoubleBuffered = True
        TextAlign = HorizontalAlignment.Center
        ForeColor = Color.FromArgb(231, 76, 60)
        Font = GlobalFont(FontStyle.Regular, 10)
        Size = New Size(130, 22)
        Enabled = True
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As PaintEventArgs)

        G = e.Graphics
        G.SmoothingMode = SmoothingMode.HighQuality
        G.TextRenderingHint = TextRenderingHint.ClearTypeGridFit

        MyBase.OnPaint(e)

        G.Clear(Parent.BackColor)

        If Enabled Then

            TB.ForeColor = Color.FromArgb(231, 76, 60)

            If State = MouseState.Down Then
                DrawRoundRect(G, FullRectangle(Size, True), 3, Color.FromArgb(231, 76, 60))
            Else
                DrawRoundRect(G, FullRectangle(Size, True), 3, GreyColor(100))
            End If

        Else
            TB.ForeColor = GreyColor(170)
            DrawRoundRect(G, FullRectangle(Size, True), 3, GreyColor(100))
        End If

        TB.TextAlign = TextAlign
        TB.UseSystemPasswordChar = UseSystemPasswordChar

    End Sub

    Protected Overrides Sub OnResize(e As EventArgs)
        MyBase.OnResize(e)
        If Not MultiLine Then
            Dim tbheight As Integer = TB.Height
            TB.Location = New Point(10, CType(((Height / 2) - (tbheight / 2) - 0), Integer))
            TB.Size = New Size(Width - 20, tbheight)
        Else
            TB.Location = New Point(10, 10)
            TB.Size = New Size(Width - 20, Height - 20)
        End If
    End Sub

    Protected Overrides Sub OnEnter(e As EventArgs)
        MyBase.OnEnter(e)
        State = MouseState.Down : Invalidate()
    End Sub

    Protected Overrides Sub OnLeave(e As EventArgs)
        MyBase.OnLeave(e)
        State = MouseState.None : Invalidate()
    End Sub

#End Region

End Class