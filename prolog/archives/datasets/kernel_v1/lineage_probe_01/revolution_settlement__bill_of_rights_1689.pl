% ============================================================================
% CONSTRAINT STORY: revolution_settlement__bill_of_rights_1689
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_revolution_settlement__bill_of_rights_1689, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: revolution_settlement__bill_of_rights_1689
 *   human_readable: Bill of Rights 1689: Parliamentary Privilege vs. Crown Prerogative
 *   domain: political/constitutional_law
 *
 * SUMMARY:
 *   The Bill of Rights 1689 is one institutional expression of the revolution
 *   settlement that ended James II's reign and established William and Mary
 *   as joint sovereigns. The Bill explicitly prohibits the Crown from
 *   suspending and dispensing with laws, secures parliamentary privilege and
 *   freedom of speech, requires parliamentary consent for taxation, and
 *   reframes the Crown's powers as parliamentary-dependent. This constraint
 *   story models the Bill as ONE READING of the contested settlement kernel —
 *   specifically, the reading emphasizing parliamentary privilege and
 *   suppression of prerogative legislation. The kernel itself (the settlement
 *   order of 1688–89) is contested across three major readings: the Bill of
 *   Rights 1689 (parliamentary privilege and prerogative suppression), the
 *   Act of Settlement 1701 (succession by statute and Crown as parliamentary
 *   office), and the Toleration Act 1689 (religious accommodation and
 *   calibrated exclusion). Each reading represents a different structural
 *   dimension of the settlement and would yield different constraint
 *   classifications. This story instantiates the Bill reading, modeling how
 *   parliamentary agents experience the constraint as coordination while
 *   subjects experience it as precarious protection dependent on enforcement,
 *   how the Crown experiences it as immutable rewriting of fundamental law
 *   (false summit), and how analysts see it as tangled between coordination
 *   (stable rules enabling cooperation) and extraction (privileging
 *   Protestant propertied interests while excluding others). The constraint
 *   exhibits significant temporal drift: extractiveness declines from 0.52
 *   (immediately post-1689, when Crown compliance was uncertain) to 0.35 (by
 *   early 18th century, as parliamentary authority normalized); theater ratio
 *   rises from 0.35 to 0.48 as the constraint shifts from actively enforced
 *   political commitment to institutionalized ritual affirming
 *   already-settled supremacy; suppression requirement declines from 0.75 to
 *   0.62 as active enforcement pressure eases and the constraint becomes
 *   embedded in constitutional practice.
 *
 * KEY AGENTS:
 *   - Parliament (Whig collective): Institutional/arbitrage — primary beneficiary. The Bill secures parliamentary privilege, control over legislation and taxation, and reframes the Crown as accountable to parliament. Experiences the constraint as pure coordination enabling parliamentary function.
 *   - Crown Authority (William and successors): Institutional/constrained — victim of prerogative suppression yet also beneficiary of settlement legitimacy. Experiences the constraint as rewriting fundamental terms of monarchy, with practical enforcement costs.
 *   - Protestant Propertied Class (gentry, merchants): Moderate/constrained — secondary beneficiary. Gains secure property rights, religious settlement confirming Protestant succession, and stable rules for commerce and governance. Also experiences extraction through privileged status requiring maintenance.
 *   - General Subjects (especially non-Protestants): Powerless/trapped — formal beneficiaries of prohibition on dispensing yet practically dependent on parliamentary enforcement and absent from decision-making. Experience precarious protection and continued vulnerability to arbitrary power exercised through other channels.
 *   - Analytical Observer: Analytical/analytical — identifies the constraint as tangled (both coordination enabling cooperation and extraction privileging sectarian interests) and detects false summit in Crown's perception of constitutional immutability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(revolution_settlement__bill_of_rights_1689, 0.35).
domain_priors:suppression_score(revolution_settlement__bill_of_rights_1689, 0.62).
domain_priors:theater_ratio(revolution_settlement__bill_of_rights_1689, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(revolution_settlement__bill_of_rights_1689, extractiveness, 0.35).
narrative_ontology:constraint_metric(revolution_settlement__bill_of_rights_1689, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(revolution_settlement__bill_of_rights_1689, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(revolution_settlement__bill_of_rights_1689, tangled_rope).
narrative_ontology:human_readable(revolution_settlement__bill_of_rights_1689, "Bill of Rights 1689: Parliamentary Privilege vs. Crown Prerogative").
narrative_ontology:topic_domain(revolution_settlement__bill_of_rights_1689, "political/constitutional_law").

domain_priors:requires_active_enforcement(revolution_settlement__bill_of_rights_1689).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(revolution_settlement__bill_of_rights_1689, '8b410783-f9ef-4215-9111-8b6d6e352fb9').
narrative_ontology:cs_kernel_codification('8b410783-f9ef-4215-9111-8b6d6e352fb9', formalized).
narrative_ontology:cs_authority_grounding('8b410783-f9ef-4215-9111-8b6d6e352fb9', lineage).
narrative_ontology:cs_interpretation_layer_present('8b410783-f9ef-4215-9111-8b6d6e352fb9').
narrative_ontology:cs_reading_relation('8b410783-f9ef-4215-9111-8b6d6e352fb9', revolution_settlement__act_of_settlement_1701, influences).
narrative_ontology:cs_reading_relation('8b410783-f9ef-4215-9111-8b6d6e352fb9', revolution_settlement__toleration_settlement_1689, influences).
narrative_ontology:cs_axiom('8b410783-f9ef-4215-9111-8b6d6e352fb9', foundational, parliamentary_sovereignty_foundational).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('8b410783-f9ef-4215-9111-8b6d6e352fb9', parliamentary_sovereignty_foundational, deontological).
narrative_ontology:cs_axiom('8b410783-f9ef-4215-9111-8b6d6e352fb9', foundational, prerogative_suppression_non_dispensing).
narrative_ontology:cs_axiom_status(prerogative_suppression_non_dispensing, holdable).
narrative_ontology:cs_axiom_grounding('8b410783-f9ef-4215-9111-8b6d6e352fb9', prerogative_suppression_non_dispensing, empirically_contingent).
narrative_ontology:cs_reference_frame('8b410783-f9ef-4215-9111-8b6d6e352fb9', parliamentary_supremacy_with_secured_prerogative_suppression).
narrative_ontology:cs_drift_state('8b410783-f9ef-4215-9111-8b6d6e352fb9', constitutional_normalization_1729, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b410783-f9ef-4215-9111-8b6d6e352fb9', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(revolution_settlement__bill_of_rights_1689, revolution_settlement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(revolution_settlement__bill_of_rights_1689, parliament_members).
narrative_ontology:constraint_beneficiary(revolution_settlement__bill_of_rights_1689, parliamentary_privilege).
narrative_ontology:constraint_victim(revolution_settlement__bill_of_rights_1689, crown_prerogative_authority).
narrative_ontology:constraint_victim(revolution_settlement__bill_of_rights_1689, monarchical_dispensing_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT UNDER DISPENSING POWER (SNARE) — Before 1689, subjects faced the Crown's arbitrary dispensing and suspending powers with no exit and no remedy. The constraint binds them absolutely. Post-1689, the Bill formally prohibits dispensing — yet enforcement requires parliamentary will and continued militant vigilance. Suppression remains high because the Crown retains practical capacity to ignore parliament; subjects remain trapped between formal protection and actual vulnerability. Maximum extraction because the subject's relief is contingent on forces beyond their control.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROTESTANT PROPERTIED CLASS (TANGLED ROPE) — Benefits from parliamentary confirmation of property rights and freedom from arbitrary seizure; also benefits from religious settlement securing Protestant succession. Experiences genuine coordination: the Bill secures stable rules for property and commerce. But also experiences extraction: the Bill privileges Protestant interests while excluding Catholic ones; propertied class benefits from the settlement's privileged position within the framework. Mixed experience — significant coordination benefit with embedded asymmetric extraction favoring their religious and economic position.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENT COLLECTIVE (ROPE) — Primary beneficiary. The Bill secures parliamentary privilege (speech immunity), confirms control over taxation and legislation, and reframes the Crown as a parliamentary office. Parliament experiences the constraint as pure coordination: it solves the collective action problem of preventing the Crown from using dispensing and suspending powers to dissolve parliamentary cooperation. The Bill enables parliament to function as a stable deliberative body. Net beneficiary — extraction flows toward parliament, not away.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CROWN AUTHORITY POST-1689 (MOUNTAIN) — From the Crown's perspective at generational horizons, the Bill appears to rewrite the fundamental terms of monarchy: prerogative is not abolished but constrained by a new natural law of constitutional government. The Crown experiences the constraint as an immutable restructuring — the king cannot rule by decree, Parliament cannot be bypassed, taxes require consent. From this view, the Bill is not a negotiated truce but a new constitutional physics. The constraint appears as a fixed law of the realm. However, this is a false summit — the Crown's experienced immutability masks the contingent political victory of the Whig coalition and the enforcement costs required to maintain the constraint.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RITUAL AFFIRMATION OF PARLIAMENTARY SUPREMACY (PITON) — From the longest temporal view, the Bill of Rights functions largely as a performative ritual by the 19th–20th centuries. Parliament has become the dominant authority; the Crown's prerogative is attenuated; dispensing and suspending powers are historical artifacts. The constraint persists through institutional inertia and symbolic affirmation rather than active enforcement. The bill is regularly invoked in ceremony and legal argument but functions primarily to legitimize parliamentary supremacy that is already dominant through other mechanisms. Theater ratio is moderate (0.48) because the constraint still carries some functional force in constitutional argument, but by the civilizational horizon, much of its force is theatrical maintenance of an already-settled order.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The Bill of Rights, analyzed as a constraint on the exercise of authority, exhibits both coordination and extraction. Coordination function: it provides stable rules for governance that enable economic, religious, and political cooperation by removing the threat of arbitrary power. Extraction function: it privileges Protestant English propertied interests while excluding others (Catholics, non-property-owners, colonized subjects); it establishes parliamentary privilege as a supremacy claim that depends on military force (the settlement was backed by armed resistance and foreign intervention). The constraint is tangled because removing the extraction mechanism (religious discrimination, propertied restriction) would dissolve the coordination function (the Bill's legitimacy rested on the specific coalition that opposed James II). The analytical view requires both dimensions for accurate structural classification.
constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(revolution_settlement__bill_of_rights_1689_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(revolution_settlement__bill_of_rights_1689, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(revolution_settlement__bill_of_rights_1689, TR),
    TR >= 0.70.

:- end_tests(revolution_settlement__bill_of_rights_1689_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Bill explicitly suppresses the Crown's most direct extraction mechanism (dispensing and suspending powers) and secures parliamentary control over taxation. Yet extractiveness is not low because the constraint's benefits are asymmetrically distributed: the propertied Protestant class gains stable property rights and religious confirmation; non-Protestants and non-property-owners remain vulnerable; the constraint's enforcement depends on militant parliamentary vigilance, which itself extracts costs. The value of 0.35 reflects that the primary extraction mechanism targeted by the Bill (arbitrary dispensing) is substantially suppressed, but secondary extraction mechanisms (religious discrimination, property qualification, parliamentary privilege becoming supremacy) persist. The declining trajectory (0.52 → 0.35) reflects the normalization of parliamentary authority: as parliament's dominance becomes institutionally embedded, the constraint's burden of active enforcement eases, reducing the extraction cost. Suppression (0.62): Moderate-high. The Bill's provisions are suppressed in practice through several mechanisms: (1) the Crown retains theoretical prerogative and occasional capacity to circumvent parliamentary will; (2) enforcement depends on parliamentary mobilization and the implicit threat of renewed conflict; (3) subjects excluded from parliamentary representation (Catholics, non-property-owners, women) remain vulnerable to prerogative or arbitrary power exercised through other channels; (4) the settlement's legitimacy depends on continuous affirmation through ceremony and legal argument, meaning alternatives (restoration of absolute prerogative, negotiated return of James) must be actively prevented. Suppression declines modestly (0.75 → 0.62) as parliamentary authority becomes the unquestioned default and the threat of Crown reversion diminishes. Theater ratio (0.48): Moderate. Initially low (0.35) because the constraint carries urgent functional force — the Bill's provisions must be actively enforced to prevent Crown circumvention. By the early 18th century, theater ratio rises (0.48) because the constraint has become embedded in constitutional ritual and symbolic affirmation. Parliament ritually invokes parliamentary privilege and the prohibition on dispensing, but the Crown's actual capacity to violate these provisions is attenuated. The constraint has transitioned from actively enforced political commitment to institutionalized theatrical affirmation of parliamentary supremacy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives reveal a radical perspectival gap between parliamentary agents (who experience coordination) and subjects (who experience precarious protection). Parliament sees the Bill as solving the collective action problem of preventing Crown dissolution of parliamentary cooperation — a pure coordination mechanism (Rope). The propertied Protestant class sees mixed benefits and burdens (Tangled Rope) — gaining secure property and religious settlement while maintaining privileged position. Subjects see no relief from vulnerability (Snare) — the Bill formally prohibits dispensing but enforcement remains contingent and unavailable to those outside parliamentary representation. The Crown experiences the constraint as immutable rewriting of fundamental law (Mountain/false summit), when in fact the constraint's force depends on contingent political enforcement and would dissolve if parliamentary coalition fragmented. The analytical observer identifies the constraint as genuinely tangled: it coordinates governance by removing arbitrary prerogative but extracts through privileged sectarian distribution. The longest temporal view (piton perspective) sees the Bill as increasingly performative — the constraint's force is largely ceremonial by the 19th century because parliamentary supremacy is already institutionally dominant through other mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the constraint. Parliament benefits from the constraint (suppression of prerogative that would dissolve parliamentary cooperation) and has arbitrage options (could negotiate alternative arrangements with the Crown, could use armed force to compel Crown compliance). This produces low d → negative f(d) → experienced chi as low or favorable. The propertied Protestant class benefits from the constraint (secure property, religious settlement) and has constrained exit (could lose property or religious security through Crown reversion). This produces moderate d → moderate f(d) → experienced chi as moderate coordination benefit with embedded extraction. Subjects have no exit options (trapped) and are largely victims of the constraint's limitations (formal prohibition on dispensing does not extend practical protection without parliamentary enforcement available to them). This produces high d → high f(d) → experienced chi as high extraction. The Crown is nominally a victim (prerogative suppressed) yet derives legitimacy and practical governance authority from the settlement. This produces intermediate d (partially beneficiary through legitimacy, partially victim through prerogative loss) → intermediate f(d) → experienced chi as moderate burden that reframes as necessity. The agent_power assignments (Parliament: institutional, propertied class: moderate, subjects: powerless, Crown: institutional) reflect constraint-relative power — the subject's powerlessness is relative to the constraint's enforcement mechanisms, not their absolute global status.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_ambiguity,
    'Is the Bill of Rights enforced primarily through formal legal prohibition (mountain-like immutability) or through militant parliamentary vigilance and the threat of renewed civil conflict (contingent political enforcement)?',
    'Historical analysis of enforcement capacity: tracing instances of Crown attempts to circumvent the Bill and the response mechanisms (parliamentary mobilization, armed threat, constitutional litigation). Comparison of formal vs. actual enforceability across the 17th–20th centuries.',
    'If formal gate applies: Bill is closer to mountain (immutable law of the realm). If militant enforcement required: Bill is closer to snare or tangled rope (requires continuous active suppression of alternatives). Historical record shows both mechanisms co-present; their relative weight changed over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, empirical, 'Whether Bill''s enforcement is formal-legal or political-militant').

omega_variable(
    religious_dispensation_scope,
    'Does the Bill of Rights'' prohibition on dispensing apply universally to all subjects, or only to the Protestant propertied elite whose interests it protects?',
    'Empirical examination of subsequent Crown attempts to dispense laws in religious matters (especially post-1707 Union with Scotland) and the differential treatment of Catholic vs. Protestant populations. Analysis of which subjects experienced the Bill as binding constraint vs. those excluded from its protection.',
    'If universal application: the Bill is a genuine constraint on arbitrary power (rope/tangled_rope toward all subjects). If selective application: the Bill is an asymmetric coordination mechanism benefiting Protestants while leaving non-Protestants exposed to arbitrary power (snare from non-Protestant perspective, rope from Protestant perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_dispensation_scope, empirical, 'Whether dispensing prohibition applies universally or selectively by religion').

omega_variable(
    settlement_kernel_identity,
    'Is this constraint (Bill of Rights 1689) the defining charter of the revolution settlement, or is it ONE institutional expression of a settlement whose kernel lies elsewhere (Act of Settlement 1701 on succession, Toleration Act 1689 on religious accommodation)?',
    'Historical and textual analysis of which document the settlement parties (William, parliament, clergy) treated as foundational, whose violation triggered the greatest threat of renewed conflict, and which reappears in subsequent legitimacy claims. Cross-examination with sibling reading contexts (Act of Settlement, Toleration).',
    'If Bill is foundational: the settlement''s primary binding constraint is parliamentary privilege and suppression of prerogative. If Act of Settlement is foundational: the settlement''s primary binding constraint is succession by statute and parliamentary authority over Crown office. If Toleration is foundational: the settlement''s primary binding constraint is religious accommodation. Each reading yields different extracted values and different victim/beneficiary sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_kernel_identity, conceptual, 'Which reading defines the settlement''s kernel identity').

omega_variable(
    dispensing_power_extinction_timeline,
    'When was the Crown''s capacity to dispense and suspend laws actually extinguished — at 1689 (Bill''s promulgation), at 1701 (Act of Settlement), during the 18th century gradual normalization, or never (retained as theoretical prerogative)?',
    'Chronological documentation of Crown attempts to use dispensing/suspending powers post-1689, parliamentary responses, and the point at which such attempts ceased to be credible threats. Examination of legal arguments in constitutional cases (Entick v. Carrington, Marbury v. Madison parallels) and governmental practice.',
    'If extinction at 1689: the Bill''s extractiveness (0.35) understates the constraint''s immediate force. If extinction delayed to 1701 or later: the Bill''s extractiveness overstates its actual binding force in the years immediately following. If extinction is theoretical (prerogative retained but politically unusable): the Bill is genuinely piton-like in its later history.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dispensing_power_extinction_timeline, empirical, 'Timeline of actual extinction of dispensing and suspending powers').

omega_variable(
    false_summit_detection_false_foundational_law,
    'Does the Bill of Rights, when described as a ''charter'' or ''law of the realm,'' naturalize a politically contingent settlement negotiated by armed conflict and foreign intervention?',
    'Comparative analysis: tracing how post-1689 constitutional discourse treats the Bill (as immutable natural law vs. as pragmatic negotiated settlement). Examining whether Crown advocates attempted to circumvent the Bill''s provisions and what rhetoric they deployed. Analyzing whether the Bill is presented as ''discovered'' (mountain-like) or ''established'' (political/contingent) in contemporaneous and subsequent legal/political argument.',
    'If Bill is genuinely natural law: appropriate to classify from analytical perspective as mountain (false summit candidate only if beneficiaries are declared). If Bill naturalizes political victory: classification should remain tangled_rope even from civilizational analytical view — the apparent immutability is a false summit masking extraction. This omega determines whether the mountain classification (Perspective 4) is correctly identified as a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_false_foundational_law, conceptual, 'Whether the Bill''s naturalization is genuine or rhetorical false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(revolution_settlement__bill_of_rights_1689, 1689, 1729).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(br1689_theater_initial, revolution_settlement__bill_of_rights_1689, theater_ratio, 0, 0.35).
narrative_ontology:measurement(br1689_theater_mid, revolution_settlement__bill_of_rights_1689, theater_ratio, 20, 0.42).
narrative_ontology:measurement(br1689_theater_late, revolution_settlement__bill_of_rights_1689, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(br1689_extract_initial, revolution_settlement__bill_of_rights_1689, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(br1689_extract_mid, revolution_settlement__bill_of_rights_1689, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(br1689_extract_late, revolution_settlement__bill_of_rights_1689, base_extractiveness, 40, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(br1689_suppression_initial, revolution_settlement__bill_of_rights_1689, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(br1689_suppression_mid, revolution_settlement__bill_of_rights_1689, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(br1689_suppression_late, revolution_settlement__bill_of_rights_1689, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(revolution_settlement__bill_of_rights_1689, enforcement_mechanism).
narrative_ontology:affects_constraint(revolution_settlement__bill_of_rights_1689, revolution_settlement__act_of_settlement_1701).
narrative_ontology:affects_constraint(revolution_settlement__bill_of_rights_1689, revolution_settlement__toleration_settlement_1689).

% DUAL FORMULATION NOTE:
% The Bill of Rights 1689 is one constraint instantiation of the contested revolution settlement kernel. The settlement itself has three structural dimensions (parliamentary privilege, succession/Crown authority, religious accommodation), each captured by a distinct sibling reading. The three readings are linked by network edges indicating mutual influence: the Bill reading's success constrains the Act and Toleration readings' scope; the Toleration reading constrains the Bill's religious enforcement claims. Decomposition is required by ε-invariance: the Bill's extractiveness (0.35) focuses on suppression of prerogative; the Act's extractiveness would focus on succession and Crown reframing; the Toleration's extractiveness would focus on religious mechanism. Each exhibits different base properties and temporal trajectories. The readings coexist (same historical moment) yet represent different agent coalitions' interpretations of the settlement's binding mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(revolution_settlement__bill_of_rights_1689, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
