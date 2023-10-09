module React.HTMLAttributes where

import Data.Undefined.NoProblem (Opt)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type AriaAttributes' otherProps =
  (
--     /** Identifies the currently active element when DOM focus is on a composite widget, textbox, group, or application. */
--     'aria-activedescendant'?: string | undefined;
--     /** Indicates whether assistive technologies will present all, or only parts of, the changed region based on the change notifications defined by the aria-relevant attribute. */
--     'aria-atomic'?: Booleanish | undefined;
--     /**
--      * Indicates whether inputting text could trigger display of one or more predictions of the user's intended value for an input and specifies how predictions would be
--      * presented if they are made.
--      */
--     'aria-autocomplete'?: 'none' | 'inline' | 'list' | 'both' | undefined;
--     /** Indicates an element is being modified and that assistive technologies MAY want to wait until the modifications are complete before exposing them to the user. */
--     'aria-busy'?: Booleanish | undefined;
--     /**
--      * Indicates the current "checked" state of checkboxes, radio buttons, and other widgets.
--      * @see aria-pressed @see aria-selected.
--      */
--     'aria-checked'?: boolean | 'false' | 'mixed' | 'true' | undefined;
--     /**
--      * Defines the total number of columns in a table, grid, or treegrid.
--      * @see aria-colindex.
--      */
--     'aria-colcount'?: number | undefined;
--     /**
--      * Defines an element's column index or position with respect to the total number of columns within a table, grid, or treegrid.
--      * @see aria-colcount @see aria-colspan.
--      */
--     'aria-colindex'?: number | undefined;
--     /**
--      * Defines the number of columns spanned by a cell or gridcell within a table, grid, or treegrid.
--      * @see aria-colindex @see aria-rowspan.
--      */
--     'aria-colspan'?: number | undefined;
--     /**
--      * Identifies the element (or elements) whose contents or presence are controlled by the current element.
--      * @see aria-owns.
--      */
--     'aria-controls'?: string | undefined;
--     /** Indicates the element that represents the current item within a container or set of related elements. */
--     'aria-current'?: boolean | 'false' | 'true' | 'page' | 'step' | 'location' | 'date' | 'time' | undefined;
--     /**
--      * Identifies the element (or elements) that describes the object.
--      * @see aria-labelledby
--      */
--     'aria-describedby'?: string | undefined;
--     /**
--      * Identifies the element that provides a detailed, extended description for the object.
--      * @see aria-describedby.
--      */
--     'aria-details'?: string | undefined;
--     /**
--      * Indicates that the element is perceivable but disabled, so it is not editable or otherwise operable.
--      * @see aria-hidden @see aria-readonly.
--      */
--     'aria-disabled'?: Booleanish | undefined;
--     /**
--      * Indicates what functions can be performed when a dragged object is released on the drop target.
--      * @deprecated in ARIA 1.1
--      */
--     'aria-dropeffect'?: 'none' | 'copy' | 'execute' | 'link' | 'move' | 'popup' | undefined;
--     /**
--      * Identifies the element that provides an error message for the object.
--      * @see aria-invalid @see aria-describedby.
--      */
--     'aria-errormessage'?: string | undefined;
--     /** Indicates whether the element, or another grouping element it controls, is currently expanded or collapsed. */
--     'aria-expanded'?: Booleanish | undefined;
--     /**
--      * Identifies the next element (or elements) in an alternate reading order of content which, at the user's discretion,
--      * allows assistive technology to override the general default of reading in document source order.
--      */
--     'aria-flowto'?: string | undefined;
--     /**
--      * Indicates an element's "grabbed" state in a drag-and-drop operation.
--      * @deprecated in ARIA 1.1
--      */
--     'aria-grabbed'?: Booleanish | undefined;
--     /** Indicates the availability and type of interactive popup element, such as menu or dialog, that can be triggered by an element. */
--     'aria-haspopup'?: boolean | 'false' | 'true' | 'menu' | 'listbox' | 'tree' | 'grid' | 'dialog' | undefined;
--     /**
--      * Indicates whether the element is exposed to an accessibility API.
--      * @see aria-disabled.
--      */
--     'aria-hidden'?: Booleanish | undefined;
--     /**
--      * Indicates the entered value does not conform to the format expected by the application.
--      * @see aria-errormessage.
--      */
--     'aria-invalid'?: boolean | 'false' | 'true' | 'grammar' | 'spelling' | undefined;
--     /** Indicates keyboard shortcuts that an author has implemented to activate or give focus to an element. */
--     'aria-keyshortcuts'?: string | undefined;
--     /**
--      * Defines a string value that labels the current element.
--      * @see aria-labelledby.
--      */
    "aria-label" :: Opt String --     'aria-label'?: string | undefined;
--     /**
--      * Identifies the element (or elements) that labels the current element.
--      * @see aria-describedby.
--      */
  , "aria-labeledby" :: Opt String --     'aria-labelledby'?: string | undefined;
--     /** Defines the hierarchical level of an element within a structure. */
--     'aria-level'?: number | undefined;
--     /** Indicates that an element will be updated, and describes the types of updates the user agents, assistive technologies, and user can expect from the live region. */
--     'aria-live'?: 'off' | 'assertive' | 'polite' | undefined;
--     /** Indicates whether an element is modal when displayed. */
--     'aria-modal'?: Booleanish | undefined;
--     /** Indicates whether a text box accepts multiple lines of input or only a single line. */
--     'aria-multiline'?: Booleanish | undefined;
--     /** Indicates that the user may select more than one item from the current selectable descendants. */
--     'aria-multiselectable'?: Booleanish | undefined;
--     /** Indicates whether the element's orientation is horizontal, vertical, or unknown/ambiguous. */
--     'aria-orientation'?: 'horizontal' | 'vertical' | undefined;
--     /**
--      * Identifies an element (or elements) in order to define a visual, functional, or contextual parent/child relationship
--      * between DOM elements where the DOM hierarchy cannot be used to represent the relationship.
--      * @see aria-controls.
--      */
--     'aria-owns'?: string | undefined;
--     /**
--      * Defines a short hint (a word or short phrase) intended to aid the user with data entry when the control has no value.
--      * A hint could be a sample value or a brief description of the expected format.
--      */
--     'aria-placeholder'?: string | undefined;
--     /**
--      * Defines an element's number or position in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM.
--      * @see aria-setsize.
--      */
--     'aria-posinset'?: number | undefined;
--     /**
--      * Indicates the current "pressed" state of toggle buttons.
--      * @see aria-checked @see aria-selected.
--      */
--     'aria-pressed'?: boolean | 'false' | 'mixed' | 'true' | undefined;
--     /**
--      * Indicates that the element is not editable, but is otherwise operable.
--      * @see aria-disabled.
--      */
--     'aria-readonly'?: Booleanish | undefined;
--     /**
--      * Indicates what notifications the user agent will trigger when the accessibility tree within a live region is modified.
--      * @see aria-atomic.
--      */
--     'aria-relevant'?: 'additions' | 'additions removals' | 'additions text' | 'all' | 'removals' | 'removals additions' | 'removals text' | 'text' | 'text additions' | 'text removals' | undefined;
--     /** Indicates that user input is required on the element before a form may be submitted. */
--     'aria-required'?: Booleanish | undefined;
--     /** Defines a human-readable, author-localized description for the role of an element. */
--     'aria-roledescription'?: string | undefined;
--     /**
--      * Defines the total number of rows in a table, grid, or treegrid.
--      * @see aria-rowindex.
--      */
--     'aria-rowcount'?: number | undefined;
--     /**
--      * Defines an element's row index or position with respect to the total number of rows within a table, grid, or treegrid.
--      * @see aria-rowcount @see aria-rowspan.
--      */
--     'aria-rowindex'?: number | undefined;
--     /**
--      * Defines the number of rows spanned by a cell or gridcell within a table, grid, or treegrid.
--      * @see aria-rowindex @see aria-colspan.
--      */
--     'aria-rowspan'?: number | undefined;
--     /**
--      * Indicates the current "selected" state of various widgets.
--      * @see aria-checked @see aria-pressed.
--      */
--     'aria-selected'?: Booleanish | undefined;
--     /**
--      * Defines the number of items in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM.
--      * @see aria-posinset.
--      */
--     'aria-setsize'?: number | undefined;
--     /** Indicates if items in a table or grid are sorted in ascending or descending order. */
--     'aria-sort'?: 'none' | 'ascending' | 'descending' | 'other' | undefined;
--     /** Defines the maximum allowed value for a range widget. */
--     'aria-valuemax'?: number | undefined;
--     /** Defines the minimum allowed value for a range widget. */
--     'aria-valuemin'?: number | undefined;
--     /**
--      * Defines the current value for a range widget.
--      * @see aria-valuetext.
--      */
--     'aria-valuenow'?: number | undefined;
--     /** Defines the human readable text alternative of aria-valuenow for a range widget. */
--     'aria-valuetext'?: string | undefined;
-- }
  )
-- // All the WAI-ARIA 1.1 role attribute values from https://www.w3.org/TR/wai-aria-1.1/#role_definitions
-- type AriaRole =
--     | 'alert'
--     | 'alertdialog'
--     | 'application'
--     | 'article'
--     | 'banner'
--     | 'button'
--     | 'cell'
--     | 'checkbox'
--     | 'columnheader'
--     | 'combobox'
--     | 'complementary'
--     | 'contentinfo'
--     | 'definition'
--     | 'dialog'
--     | 'directory'
--     | 'document'
--     | 'feed'
--     | 'figure'
--     | 'form'
--     | 'grid'
--     | 'gridcell'
--     | 'group'
--     | 'heading'
--     | 'img'
--     | 'link'
--     | 'list'
--     | 'listbox'
--     | 'listitem'
--     | 'log'
--     | 'main'
--     | 'marquee'
--     | 'math'
--     | 'menu'
--     | 'menubar'
--     | 'menuitem'
--     | 'menuitemcheckbox'
--     | 'menuitemradio'
--     | 'navigation'
--     | 'none'
--     | 'note'
--     | 'option'
--     | 'presentation'
--     | 'progressbar'
--     | 'radio'
--     | 'radiogroup'
--     | 'region'
--     | 'row'
--     | 'rowgroup'
--     | 'rowheader'
--     | 'scrollbar'
--     | 'search'
--     | 'searchbox'
--     | 'separator'
--     | 'slider'
--     | 'spinbutton'
--     | 'status'
--     | 'switch'
--     | 'tab'
--     | 'table'
--     | 'tablist'
--     | 'tabpanel'
--     | 'term'
--     | 'textbox'
--     | 'timer'
--     | 'toolbar'
--     | 'tooltip'
--     | 'tree'
--     | 'treegrid'
--     | 'treeitem'
--     | (string & {});




-- Based on types definitions:
-- node_modules/@types/react/index.d.ts

-- react-bootstrap components use these attribute sets
-- and they are not used by the react-basic.
-- FIXME: actually port or codegen all of them.

type DOMAttributes otherProps =
  ( children :: Array JSX
  -- , dangerouslySetInnerHTML?: {
  --       __html: string;
  --   } | undefined;

  --   // Clipboard Events
  --   onCopy?: ClipboardEventHandler<T> | undefined;
  --   onCopyCapture?: ClipboardEventHandler<T> | undefined;
  --   onCut?: ClipboardEventHandler<T> | undefined;
  --   onCutCapture?: ClipboardEventHandler<T> | undefined;
  --   onPaste?: ClipboardEventHandler<T> | undefined;
  --   onPasteCapture?: ClipboardEventHandler<T> | undefined;

  --   // Composition Events
  --   onCompositionEnd?: CompositionEventHandler<T> | undefined;
  --   onCompositionEndCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionStart?: CompositionEventHandler<T> | undefined;
  --   onCompositionStartCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdate?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdateCapture?: CompositionEventHandler<T> | undefined;

  --   // Focus Events
  --   onFocus?: FocusEventHandler<T> | undefined;
  --   onFocusCapture?: FocusEventHandler<T> | undefined;
  --   onBlur?: FocusEventHandler<T> | undefined;
  --   onBlurCapture?: FocusEventHandler<T> | undefined;

  --   // Form Events
  -- Should we expose these?
  -- Won't we get duplicates from that?
  -- , onChange :: EventHandler -- FormEventHandler<T> | undefined;
  --   onChangeCapture?: FormEventHandler<T> | undefined;
  --   onBeforeInput?: FormEventHandler<T> | undefined;
  --   onBeforeInputCapture?: FormEventHandler<T> | undefined;
  -- , onInput :: EventHandler --   onInput?: FormEventHandler<T> | undefined;
  --   onInputCapture?: FormEventHandler<T> | undefined;
  --   onReset?: FormEventHandler<T> | undefined;
  --   onResetCapture?: FormEventHandler<T> | undefined;
  --   onSubmit?: FormEventHandler<T> | undefined;
  --   onSubmitCapture?: FormEventHandler<T> | undefined;
  --   onInvalid?: FormEventHandler<T> | undefined;
  --   onInvalidCapture?: FormEventHandler<T> | undefined;

  --   // Image Events
  --   onLoad?: ReactEventHandler<T> | undefined;
  --   onLoadCapture?: ReactEventHandler<T> | undefined;
  --   onError?: ReactEventHandler<T> | undefined; // also a Media Event
  --   onErrorCapture?: ReactEventHandler<T> | undefined; // also a Media Event

  --   // Keyboard Events
  --   onKeyDown?: KeyboardEventHandler<T> | undefined;
  --   onKeyDownCapture?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPress?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPressCapture?: KeyboardEventHandler<T> | undefined;
  --   onKeyUp?: KeyboardEventHandler<T> | undefined;
  --   onKeyUpCapture?: KeyboardEventHandler<T> | undefined;

  --   // Media Events
  --   onAbort?: ReactEventHandler<T> | undefined;
  --   onAbortCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlay?: ReactEventHandler<T> | undefined;
  --   onCanPlayCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlayThrough?: ReactEventHandler<T> | undefined;
  --   onCanPlayThroughCapture?: ReactEventHandler<T> | undefined;
  --   onDurationChange?: ReactEventHandler<T> | undefined;
  --   onDurationChangeCapture?: ReactEventHandler<T> | undefined;
  --   onEmptied?: ReactEventHandler<T> | undefined;
  --   onEmptiedCapture?: ReactEventHandler<T> | undefined;
  --   onEncrypted?: ReactEventHandler<T> | undefined;
  --   onEncryptedCapture?: ReactEventHandler<T> | undefined;
  --   onEnded?: ReactEventHandler<T> | undefined;
  --   onEndedCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedData?: ReactEventHandler<T> | undefined;
  --   onLoadedDataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadata?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadStart?: ReactEventHandler<T> | undefined;
  --   onLoadStartCapture?: ReactEventHandler<T> | undefined;
  --   onPause?: ReactEventHandler<T> | undefined;
  --   onPauseCapture?: ReactEventHandler<T> | undefined;
  --   onPlay?: ReactEventHandler<T> | undefined;
  --   onPlayCapture?: ReactEventHandler<T> | undefined;
  --   onPlaying?: ReactEventHandler<T> | undefined;
  --   onPlayingCapture?: ReactEventHandler<T> | undefined;
  --   onProgress?: ReactEventHandler<T> | undefined;
  --   onProgressCapture?: ReactEventHandler<T> | undefined;
  --   onRateChange?: ReactEventHandler<T> | undefined;
  --   onRateChangeCapture?: ReactEventHandler<T> | undefined;
  --   onResize?: ReactEventHandler<T> | undefined;
  --   onResizeCapture?: ReactEventHandler<T> | undefined;
  --   onSeeked?: ReactEventHandler<T> | undefined;
  --   onSeekedCapture?: ReactEventHandler<T> | undefined;
  --   onSeeking?: ReactEventHandler<T> | undefined;
  --   onSeekingCapture?: ReactEventHandler<T> | undefined;
  --   onStalled?: ReactEventHandler<T> | undefined;
  --   onStalledCapture?: ReactEventHandler<T> | undefined;
  --   onSuspend?: ReactEventHandler<T> | undefined;
  --   onSuspendCapture?: ReactEventHandler<T> | undefined;
  --   onTimeUpdate?: ReactEventHandler<T> | undefined;
  --   onTimeUpdateCapture?: ReactEventHandler<T> | undefined;
  --   onVolumeChange?: ReactEventHandler<T> | undefined;
  --   onVolumeChangeCapture?: ReactEventHandler<T> | undefined;
  --   onWaiting?: ReactEventHandler<T> | undefined;
  --   onWaitingCapture?: ReactEventHandler<T> | undefined;

  --   // MouseEvents
  --   onAuxClick?: MouseEventHandler<T> | undefined;
  --   onAuxClickCapture?: MouseEventHandler<T> | undefined;
  --   onClick?: MouseEventHandler<T> | undefined;
  --   onClickCapture?: MouseEventHandler<T> | undefined;
  --   onContextMenu?: MouseEventHandler<T> | undefined;
  --   onContextMenuCapture?: MouseEventHandler<T> | undefined;
  --   onDoubleClick?: MouseEventHandler<T> | undefined;
  --   onDoubleClickCapture?: MouseEventHandler<T> | undefined;
  --   onDrag?: DragEventHandler<T> | undefined;
  --   onDragCapture?: DragEventHandler<T> | undefined;
  --   onDragEnd?: DragEventHandler<T> | undefined;
  --   onDragEndCapture?: DragEventHandler<T> | undefined;
  --   onDragEnter?: DragEventHandler<T> | undefined;
  --   onDragEnterCapture?: DragEventHandler<T> | undefined;
  --   onDragExit?: DragEventHandler<T> | undefined;
  --   onDragExitCapture?: DragEventHandler<T> | undefined;
  --   onDragLeave?: DragEventHandler<T> | undefined;
  --   onDragLeaveCapture?: DragEventHandler<T> | undefined;
  --   onDragOver?: DragEventHandler<T> | undefined;
  --   onDragOverCapture?: DragEventHandler<T> | undefined;
  --   onDragStart?: DragEventHandler<T> | undefined;
  --   onDragStartCapture?: DragEventHandler<T> | undefined;
  --   onDrop?: DragEventHandler<T> | undefined;
  --   onDropCapture?: DragEventHandler<T> | undefined;
  --   onMouseDown?: MouseEventHandler<T> | undefined;
  --   onMouseDownCapture?: MouseEventHandler<T> | undefined;
  --   onMouseEnter?: MouseEventHandler<T> | undefined;
  --   onMouseLeave?: MouseEventHandler<T> | undefined;
  --   onMouseMove?: MouseEventHandler<T> | undefined;
  --   onMouseMoveCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOut?: MouseEventHandler<T> | undefined;
  --   onMouseOutCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOver?: MouseEventHandler<T> | undefined;
  --   onMouseOverCapture?: MouseEventHandler<T> | undefined;
  --   onMouseUp?: MouseEventHandler<T> | undefined;
  --   onMouseUpCapture?: MouseEventHandler<T> | undefined;

  --   // Selection Events
  --   onSelect?: ReactEventHandler<T> | undefined;
  --   onSelectCapture?: ReactEventHandler<T> | undefined;

  --   // Touch Events
  --   onTouchCancel?: TouchEventHandler<T> | undefined;
  --   onTouchCancelCapture?: TouchEventHandler<T> | undefined;
  --   onTouchEnd?: TouchEventHandler<T> | undefined;
  --   onTouchEndCapture?: TouchEventHandler<T> | undefined;
  --   onTouchMove?: TouchEventHandler<T> | undefined;
  --   onTouchMoveCapture?: TouchEventHandler<T> | undefined;
  --   onTouchStart?: TouchEventHandler<T> | undefined;
  --   onTouchStartCapture?: TouchEventHandler<T> | undefined;

  --   // Pointer Events
  --   onPointerDown?: PointerEventHandler<T> | undefined;
  --   onPointerDownCapture?: PointerEventHandler<T> | undefined;
  --   onPointerMove?: PointerEventHandler<T> | undefined;
  --   onPointerMoveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerUp?: PointerEventHandler<T> | undefined;
  --   onPointerUpCapture?: PointerEventHandler<T> | undefined;
  --   onPointerCancel?: PointerEventHandler<T> | undefined;
  --   onPointerCancelCapture?: PointerEventHandler<T> | undefined;
  --   onPointerEnter?: PointerEventHandler<T> | undefined;
  --   onPointerEnterCapture?: PointerEventHandler<T> | undefined;
  --   onPointerLeave?: PointerEventHandler<T> | undefined;
  --   onPointerLeaveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOver?: PointerEventHandler<T> | undefined;
  --   onPointerOverCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOut?: PointerEventHandler<T> | undefined;
  --   onPointerOutCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCaptureCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCaptureCapture?: PointerEventHandler<T> | undefined;

  --   // UI Events
  --   onScroll?: UIEventHandler<T> | undefined;
  --   onScrollCapture?: UIEventHandler<T> | undefined;

  --   // Wheel Events
  --   onWheel?: WheelEventHandler<T> | undefined;
  --   onWheelCapture?: WheelEventHandler<T> | undefined;

  --   // Animation Events
  --   onAnimationStart?: AnimationEventHandler<T> | undefined;
  --   onAnimationStartCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationEnd?: AnimationEventHandler<T> | undefined;
  --   onAnimationEndCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationIteration?: AnimationEventHandler<T> | undefined;
  --   onAnimationIterationCapture?: AnimationEventHandler<T> | undefined;

  --   // Transition Events
  --   onTransitionEnd?: TransitionEventHandler<T> | undefined;
  --   onTransitionEndCapture?: TransitionEventHandler<T> | undefined;
  | otherProps
  )

type DOMAttributes' otherProps =
  ( children :: Opt (Array JSX)
  -- , dangerouslySetInnerHTML?: {
  --       __html: string;
  --   } | undefined;

  --   // Clipboard Events
  --   onCopy?: ClipboardEventHandler<T> | undefined;
  --   onCopyCapture?: ClipboardEventHandler<T> | undefined;
  --   onCut?: ClipboardEventHandler<T> | undefined;
  --   onCutCapture?: ClipboardEventHandler<T> | undefined;
  --   onPaste?: ClipboardEventHandler<T> | undefined;
  --   onPasteCapture?: ClipboardEventHandler<T> | undefined;

  --   // Composition Events
  --   onCompositionEnd?: CompositionEventHandler<T> | undefined;
  --   onCompositionEndCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionStart?: CompositionEventHandler<T> | undefined;
  --   onCompositionStartCapture?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdate?: CompositionEventHandler<T> | undefined;
  --   onCompositionUpdateCapture?: CompositionEventHandler<T> | undefined;

  --   // Focus Events
  --   onFocus?: FocusEventHandler<T> | undefined;
  --   onFocusCapture?: FocusEventHandler<T> | undefined;
  --   onBlur?: FocusEventHandler<T> | undefined;
  --   onBlurCapture?: FocusEventHandler<T> | undefined;

  --   // Form Events
  -- Should we expose these?
  -- Won't we get duplicates from that?
  -- , onChange :: EventHandler -- FormEventHandler<T> | undefined;
  --   onChangeCapture?: FormEventHandler<T> | undefined;
  --   onBeforeInput?: FormEventHandler<T> | undefined;
  --   onBeforeInputCapture?: FormEventHandler<T> | undefined;
  -- , onInput :: EventHandler --   onInput?: FormEventHandler<T> | undefined;
  --   onInputCapture?: FormEventHandler<T> | undefined;
  --   onReset?: FormEventHandler<T> | undefined;
  --   onResetCapture?: FormEventHandler<T> | undefined;
  --   onSubmit?: FormEventHandler<T> | undefined;
  --   onSubmitCapture?: FormEventHandler<T> | undefined;
  --   onInvalid?: FormEventHandler<T> | undefined;
  --   onInvalidCapture?: FormEventHandler<T> | undefined;

  --   // Image Events
  --   onLoad?: ReactEventHandler<T> | undefined;
  --   onLoadCapture?: ReactEventHandler<T> | undefined;
  --   onError?: ReactEventHandler<T> | undefined; // also a Media Event
  --   onErrorCapture?: ReactEventHandler<T> | undefined; // also a Media Event

  --   // Keyboard Events
  --   onKeyDown?: KeyboardEventHandler<T> | undefined;
  --   onKeyDownCapture?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPress?: KeyboardEventHandler<T> | undefined;
  --   /** @deprecated */
  --   onKeyPressCapture?: KeyboardEventHandler<T> | undefined;
  --   onKeyUp?: KeyboardEventHandler<T> | undefined;
  --   onKeyUpCapture?: KeyboardEventHandler<T> | undefined;

  --   // Media Events
  --   onAbort?: ReactEventHandler<T> | undefined;
  --   onAbortCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlay?: ReactEventHandler<T> | undefined;
  --   onCanPlayCapture?: ReactEventHandler<T> | undefined;
  --   onCanPlayThrough?: ReactEventHandler<T> | undefined;
  --   onCanPlayThroughCapture?: ReactEventHandler<T> | undefined;
  --   onDurationChange?: ReactEventHandler<T> | undefined;
  --   onDurationChangeCapture?: ReactEventHandler<T> | undefined;
  --   onEmptied?: ReactEventHandler<T> | undefined;
  --   onEmptiedCapture?: ReactEventHandler<T> | undefined;
  --   onEncrypted?: ReactEventHandler<T> | undefined;
  --   onEncryptedCapture?: ReactEventHandler<T> | undefined;
  --   onEnded?: ReactEventHandler<T> | undefined;
  --   onEndedCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedData?: ReactEventHandler<T> | undefined;
  --   onLoadedDataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadata?: ReactEventHandler<T> | undefined;
  --   onLoadedMetadataCapture?: ReactEventHandler<T> | undefined;
  --   onLoadStart?: ReactEventHandler<T> | undefined;
  --   onLoadStartCapture?: ReactEventHandler<T> | undefined;
  --   onPause?: ReactEventHandler<T> | undefined;
  --   onPauseCapture?: ReactEventHandler<T> | undefined;
  --   onPlay?: ReactEventHandler<T> | undefined;
  --   onPlayCapture?: ReactEventHandler<T> | undefined;
  --   onPlaying?: ReactEventHandler<T> | undefined;
  --   onPlayingCapture?: ReactEventHandler<T> | undefined;
  --   onProgress?: ReactEventHandler<T> | undefined;
  --   onProgressCapture?: ReactEventHandler<T> | undefined;
  --   onRateChange?: ReactEventHandler<T> | undefined;
  --   onRateChangeCapture?: ReactEventHandler<T> | undefined;
  --   onResize?: ReactEventHandler<T> | undefined;
  --   onResizeCapture?: ReactEventHandler<T> | undefined;
  --   onSeeked?: ReactEventHandler<T> | undefined;
  --   onSeekedCapture?: ReactEventHandler<T> | undefined;
  --   onSeeking?: ReactEventHandler<T> | undefined;
  --   onSeekingCapture?: ReactEventHandler<T> | undefined;
  --   onStalled?: ReactEventHandler<T> | undefined;
  --   onStalledCapture?: ReactEventHandler<T> | undefined;
  --   onSuspend?: ReactEventHandler<T> | undefined;
  --   onSuspendCapture?: ReactEventHandler<T> | undefined;
  --   onTimeUpdate?: ReactEventHandler<T> | undefined;
  --   onTimeUpdateCapture?: ReactEventHandler<T> | undefined;
  --   onVolumeChange?: ReactEventHandler<T> | undefined;
  --   onVolumeChangeCapture?: ReactEventHandler<T> | undefined;
  --   onWaiting?: ReactEventHandler<T> | undefined;
  --   onWaitingCapture?: ReactEventHandler<T> | undefined;

  --   // MouseEvents
  --   onAuxClick?: MouseEventHandler<T> | undefined;
  --   onAuxClickCapture?: MouseEventHandler<T> | undefined;
  , onClick :: Opt EventHandler -- onClick?: MouseEventHandler<T> | undefined;
  --   onClickCapture?: MouseEventHandler<T> | undefined;
  --   onContextMenu?: MouseEventHandler<T> | undefined;
  --   onContextMenuCapture?: MouseEventHandler<T> | undefined;
  --   onDoubleClick?: MouseEventHandler<T> | undefined;
  --   onDoubleClickCapture?: MouseEventHandler<T> | undefined;
  --   onDrag?: DragEventHandler<T> | undefined;
  --   onDragCapture?: DragEventHandler<T> | undefined;
  --   onDragEnd?: DragEventHandler<T> | undefined;
  --   onDragEndCapture?: DragEventHandler<T> | undefined;
  --   onDragEnter?: DragEventHandler<T> | undefined;
  --   onDragEnterCapture?: DragEventHandler<T> | undefined;
  --   onDragExit?: DragEventHandler<T> | undefined;
  --   onDragExitCapture?: DragEventHandler<T> | undefined;
  --   onDragLeave?: DragEventHandler<T> | undefined;
  --   onDragLeaveCapture?: DragEventHandler<T> | undefined;
  --   onDragOver?: DragEventHandler<T> | undefined;
  --   onDragOverCapture?: DragEventHandler<T> | undefined;
  --   onDragStart?: DragEventHandler<T> | undefined;
  --   onDragStartCapture?: DragEventHandler<T> | undefined;
  --   onDrop?: DragEventHandler<T> | undefined;
  --   onDropCapture?: DragEventHandler<T> | undefined;
  --   onMouseDown?: MouseEventHandler<T> | undefined;
  --   onMouseDownCapture?: MouseEventHandler<T> | undefined;
  --   onMouseEnter?: MouseEventHandler<T> | undefined;
  --   onMouseLeave?: MouseEventHandler<T> | undefined;
  --   onMouseMove?: MouseEventHandler<T> | undefined;
  --   onMouseMoveCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOut?: MouseEventHandler<T> | undefined;
  --   onMouseOutCapture?: MouseEventHandler<T> | undefined;
  --   onMouseOver?: MouseEventHandler<T> | undefined;
  --   onMouseOverCapture?: MouseEventHandler<T> | undefined;
  --   onMouseUp?: MouseEventHandler<T> | undefined;
  --   onMouseUpCapture?: MouseEventHandler<T> | undefined;

  --   // Selection Events
  --   onSelect?: ReactEventHandler<T> | undefined;
  --   onSelectCapture?: ReactEventHandler<T> | undefined;

  --   // Touch Events
  --   onTouchCancel?: TouchEventHandler<T> | undefined;
  --   onTouchCancelCapture?: TouchEventHandler<T> | undefined;
  --   onTouchEnd?: TouchEventHandler<T> | undefined;
  --   onTouchEndCapture?: TouchEventHandler<T> | undefined;
  --   onTouchMove?: TouchEventHandler<T> | undefined;
  --   onTouchMoveCapture?: TouchEventHandler<T> | undefined;
  --   onTouchStart?: TouchEventHandler<T> | undefined;
  --   onTouchStartCapture?: TouchEventHandler<T> | undefined;

  --   // Pointer Events
  --   onPointerDown?: PointerEventHandler<T> | undefined;
  --   onPointerDownCapture?: PointerEventHandler<T> | undefined;
  --   onPointerMove?: PointerEventHandler<T> | undefined;
  --   onPointerMoveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerUp?: PointerEventHandler<T> | undefined;
  --   onPointerUpCapture?: PointerEventHandler<T> | undefined;
  --   onPointerCancel?: PointerEventHandler<T> | undefined;
  --   onPointerCancelCapture?: PointerEventHandler<T> | undefined;
  --   onPointerEnter?: PointerEventHandler<T> | undefined;
  --   onPointerEnterCapture?: PointerEventHandler<T> | undefined;
  --   onPointerLeave?: PointerEventHandler<T> | undefined;
  --   onPointerLeaveCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOver?: PointerEventHandler<T> | undefined;
  --   onPointerOverCapture?: PointerEventHandler<T> | undefined;
  --   onPointerOut?: PointerEventHandler<T> | undefined;
  --   onPointerOutCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCapture?: PointerEventHandler<T> | undefined;
  --   onGotPointerCaptureCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCapture?: PointerEventHandler<T> | undefined;
  --   onLostPointerCaptureCapture?: PointerEventHandler<T> | undefined;

  --   // UI Events
  --   onScroll?: UIEventHandler<T> | undefined;
  --   onScrollCapture?: UIEventHandler<T> | undefined;

  --   // Wheel Events
  --   onWheel?: WheelEventHandler<T> | undefined;
  --   onWheelCapture?: WheelEventHandler<T> | undefined;

  --   // Animation Events
  --   onAnimationStart?: AnimationEventHandler<T> | undefined;
  --   onAnimationStartCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationEnd?: AnimationEventHandler<T> | undefined;
  --   onAnimationEndCapture?: AnimationEventHandler<T> | undefined;
  --   onAnimationIteration?: AnimationEventHandler<T> | undefined;
  --   onAnimationIterationCapture?: AnimationEventHandler<T> | undefined;

  --   // Transition Events
  --   onTransitionEnd?: TransitionEventHandler<T> | undefined;
  --   onTransitionEndCapture?: TransitionEventHandler<T> | undefined;
  | otherProps
  )

foreign import data InputMode :: Type

-- inputMode = "none" | "text" | "tel" | "url" | "email" | "numeric" | "decimal" | "search"
inputMode
  :: { decimal :: InputMode
     , email :: InputMode
     , none :: InputMode
     , numeric :: InputMode
     , search :: InputMode
     , tel :: InputMode
     , text :: InputMode
     , url :: InputMode
     }
inputMode =
  { none: unsafeCoerce "none" :: InputMode
  , text: unsafeCoerce "text" :: InputMode
  , tel: unsafeCoerce "tel" :: InputMode
  , url: unsafeCoerce "url" :: InputMode
  , email: unsafeCoerce "email" :: InputMode
  , numeric: unsafeCoerce "numeric" :: InputMode
  , decimal: unsafeCoerce "decimal" :: InputMode
  , search: unsafeCoerce "search" :: InputMode
  }

-- interface HTMLAttributes<T> extends AriaAttributes, DOMAttributes<T> {
type HTMLAttributes otherProps =
  (
    --       about?: string | undefined;
    --       accessKey?: string | undefined;
    --       autoCapitalize?: string | undefined;
    --       autoCorrect?: string | undefined;
    --       autoSave?: string | undefined;
    className :: String
  --       color?: string | undefined;
  --       contentEditable?: Booleanish | "inherit" | undefined;
  --       contextMenu?: string | undefined;
  --       datatype?: string | undefined;
  --       defaultChecked?: boolean | undefined;
  , defaultValue :: String -- defaultValue?: string | number | ReadonlyArray<string> | undefined;
  --       dir?: string | undefined;
  --       draggable?: Booleanish | undefined;
  --       hidden?: boolean | undefined;
  , id :: String
  --       inlist?: any;
  , inputMode :: InputMode
  --       is?: string | undefined;
  --       itemID?: string | undefined;
  --       itemProp?: string | undefined;
  --       itemRef?: string | undefined;
  --       itemScope?: boolean | undefined;
  --       itemType?: string | undefined;
  --       lang?: string | undefined;
  --       nonce?: string | undefined;
  , placeholder :: String
  --       prefix?: string | undefined;
  --       property?: string | undefined;
  --       radioGroup?: string | undefined; // <command>, <menuitem>
  --       resource?: string | undefined;
  --       results?: number | undefined;
  -- , role :: Opt String -- AriaRole | undefined;
  --       security?: string | undefined;
  --       slot?: string | undefined;
  --       spellCheck?: Booleanish | undefined;
  --       style?: CSSProperties | undefined;
  --       suppressContentEditableWarning?: boolean | undefined;
  --       suppressHydrationWarning?: boolean | undefined;
  --       tabIndex?: number | undefined;
  --       title?: string | undefined;
  --       translate?: 'yes' | 'no' | undefined;
  --       typeof?: string | undefined;
  --       unselectable?: 'on' | 'off' | undefined;
  --       vocab?: string | undefined;
  --
  | DOMAttributes + otherProps
  )

type HTMLAttributes' otherProps =
  (
    --       about?: string | undefined;
    --       accessKey?: string | undefined;
    --       autoCapitalize?: string | undefined;
    --       autoCorrect?: string | undefined;
    --       autoSave?: string | undefined;
    className :: Opt String
  --       color?: string | undefined;
  --       contentEditable?: Booleanish | "inherit" | undefined;
  --       contextMenu?: string | undefined;
  --       datatype?: string | undefined;
  --       defaultChecked?: boolean | undefined;
  , defaultValue :: Opt String -- defaultValue?: string | number | ReadonlyArray<string> | undefined;
  --       dir?: string | undefined;
  --       draggable?: Booleanish | undefined;
  --       hidden?: boolean | undefined;
  , id :: Opt String
  --       inlist?: any;
  , inputMode :: Opt InputMode
  --       is?: string | undefined;
  --       itemID?: string | undefined;
  --       itemProp?: string | undefined;
  --       itemRef?: string | undefined;
  --       itemScope?: boolean | undefined;
  --       itemType?: string | undefined;
  --       lang?: string | undefined;
  --       nonce?: string | undefined;
  , placeholder :: Opt String
  --       prefix?: string | undefined;
  --       property?: string | undefined;
  --       radioGroup?: string | undefined; // <command>, <menuitem>
  --       resource?: string | undefined;
  --       results?: number | undefined;
  , role :: Opt String
  --       security?: string | undefined;
  --       slot?: string | undefined;
  --       spellCheck?: Booleanish | undefined;
  --       style?: CSSProperties | undefined;
  --       suppressContentEditableWarning?: boolean | undefined;
  --       suppressHydrationWarning?: boolean | undefined;
  --       tabIndex?: number | undefined;
  --       title?: string | undefined;
  --       translate?: 'yes' | 'no' | undefined;
  --       typeof?: string | undefined;
  --       unselectable?: 'on' | 'off' | undefined;
  --       vocab?: string | undefined;
  --
  | AriaAttributes' + DOMAttributes' + otherProps
  )

--     interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
--         accept?: string | undefined;
--         alt?: string | undefined;
--         autoComplete?: string | undefined;
--         autoFocus?: boolean | undefined;
--         capture?: boolean | 'user' | 'environment' | undefined; // https://www.w3.org/TR/html-media-capture/#the-capture-attribute
--         checked?: boolean | undefined;
--         crossOrigin?: string | undefined;
--         disabled?: boolean | undefined;
--         enterKeyHint?: 'enter' | 'done' | 'go' | 'next' | 'previous' | 'search' | 'send' | undefined;
--         form?: string | undefined;
--         formAction?: string | undefined;
--         formEncType?: string | undefined;
--         formMethod?: string | undefined;
--         formNoValidate?: boolean | undefined;
--         formTarget?: string | undefined;
--         height?: number | string | undefined;
--         list?: string | undefined;
--         max?: number | string | undefined;
--         maxLength?: number | undefined;
--         min?: number | string | undefined;
--         minLength?: number | undefined;
--         multiple?: boolean | undefined;
--         name?: string | undefined;
--         pattern?: string | undefined;
--         placeholder?: string | undefined;
--         readOnly?: boolean | undefined;
--         required?: boolean | undefined;
--         size?: number | undefined;
--         src?: string | undefined;
--         step?: number | string | undefined;
--         type?: HTMLInputTypeAttribute | undefined;
--         value?: string | ReadonlyArray<string> | number | undefined;
--         width?: number | string | undefined;
-- 
--         onChange?: ChangeEventHandler<T> | undefined;

-- FIXME: We want to drop `Opt` from here but FormBuilder depends on that.
type InputHTMLAttributes otherProps =
  HTMLAttributes +
    ( accept :: String
    , alt :: String
    , autoComplete :: String
    , autoFocus :: Boolean
    , capture :: Boolean
    , checked :: Boolean
    , crossOrigin :: String
    , disabled :: Boolean
    , enterKeyHint :: String
    , form :: String
    , formAction :: String
    , formEncType :: String
    , formMethod :: String
    , formNoValidate :: Boolean
    , formTarget :: String
    , height :: Number
    , list :: String
    , max :: Opt Number
    , maxLength :: Opt Number
    , min :: Opt Number
    , minLength :: Opt Number
    , multiple :: Boolean
    , onInput :: EventHandler
    , onChange :: EventHandler
    , name :: String
    , pattern :: String
    , placeholder :: String
    , readOnly :: Boolean
    , required :: Boolean
    , size :: Number
    , src :: String
    , step :: Opt Number
    -- This collides with `Check` - take care of it upstream
    -- , type :: String
    , value :: String
    , width :: Number
    | otherProps
    )

--     interface InputHTMLAttributes<T> extends HTMLAttributes<T> {
--         accept?: string | undefined;
--         alt?: string | undefined;
--         autoComplete?: string | undefined;
--         capture?: boolean | 'user' | 'environment' | undefined; // https://www.w3.org/TR/html-media-capture/#the-capture-attribute
--         checked?: boolean | undefined;
--         crossOrigin?: "anonymous" | "use-credentials" | "" | undefined;
--         disabled?: boolean | undefined;
--         enterKeyHint?: 'enter' | 'done' | 'go' | 'next' | 'previous' | 'search' | 'send' | undefined;
--         form?: string | undefined;
--         formAction?: string | undefined;
--         formEncType?: string | undefined;
--         formMethod?: string | undefined;
--         formNoValidate?: boolean | undefined;
--         formTarget?: string | undefined;
--         height?: number | string | undefined;
--         list?: string | undefined;
--         max?: number | string | undefined;
--         maxLength?: number | undefined;
--         min?: number | string | undefined;
--         minLength?: number | undefined;
--         multiple?: boolean | undefined;
--         name?: string | undefined;
--         pattern?: string | undefined;
--         placeholder?: string | undefined;
--         readOnly?: boolean | undefined;
--         required?: boolean | undefined;
--         size?: number | undefined;
--         src?: string | undefined;
--         step?: number | string | undefined;
--         type?: HTMLInputTypeAttribute | undefined;
--         value?: string | ReadonlyArray<string> | number | undefined;
--         width?: number | string | undefined;
-- 
--         onChange?: ChangeEventHandler<T> | undefined;
--     }

type InputHTMLAttributes' otherProps =
  HTMLAttributes' +
    ( accept :: Opt String
    , alt :: Opt String
    , autoComplete :: Opt String
    , autoFocus :: Opt Boolean
    , capture :: Opt Boolean
    , checked :: Opt Boolean
    , crossOrigin :: Opt String
    , disabled :: Opt Boolean
    , enterKeyHint :: Opt String
    , form :: Opt String
    , formAction :: Opt String
    , formEncType :: Opt String
    , formMethod :: Opt String
    , formNoValidate :: Opt Boolean
    , formTarget :: Opt String
    , height :: Opt Number
    , list :: Opt String
    , max :: Opt Number
    , maxLength :: Opt Number
    , min :: Opt Number
    , minLength :: Opt Number
    , multiple :: Opt Boolean
    , onChange :: Opt EventHandler
    , name :: Opt String
    , pattern :: Opt String
    , placeholder :: Opt String
    , readOnly :: Opt Boolean
    , required :: Opt Boolean
    , size :: Opt Number
    , src :: Opt String
    , step :: Opt Number
    -- This collides with `Check` - take care of it upstream
    -- , type :: String
    , value :: Opt String
    , width :: Opt Number
    | otherProps
    )

type SelectHTMLAttributes otherProps =
  HTMLAttributes +
    ( autoComplete :: String
    , autoFocus :: Boolean
    , disabled :: Boolean
    , form :: String
    , multiple :: Boolean
    , name :: String
    , required :: Boolean
    , size :: Number
    , value :: String
    , onChange :: EventHandler
    | otherProps
    )

type SelectHTMLAttributes' otherProps =
  HTMLAttributes' +
    ( autoComplete :: Opt String
    , autoFocus :: Opt Boolean
    , disabled :: Opt Boolean
    , form :: Opt String
    , multiple :: Opt Boolean
    , name :: Opt String
    , required :: Opt Boolean
    , size :: Opt Number
    , value :: Opt String -- string | ReadonlyArray<string> | number | undefined;
    , onChange :: EventHandler
    | otherProps
    )

--  interface FormHTMLAttributes<T> extends HTMLAttributes<T> {
--      acceptCharset?: string | undefined;
--      action?: string | undefined;
--      autoComplete?: string | undefined;
--      encType?: string | undefined;
--      method?: string | undefined;
--      name?: string | undefined;
--      noValidate?: boolean | undefined;
--      target?: string | undefined;
--      rel?: string | undefined;
--  }

type FormHTMLAttributes otherProps =
  HTMLAttributes +
    ( acceptCharset :: String
    , action :: String
    , autoComplete :: String
    , encType :: String
    , method :: String
    , name :: String
    , noValidate :: Boolean
    , target :: String
    , rel :: String
    | otherProps
    )

type FormHTMLAttributes' otherProps =
  HTMLAttributes' +
    ( acceptCharset :: Opt String
    , action :: Opt String
    , autoComplete :: Opt String
    , encType :: Opt String
    , method :: Opt String
    , name :: Opt String
    , noValidate :: Opt Boolean
    , target :: Opt String
    , rel :: Opt String
    | otherProps
    )
