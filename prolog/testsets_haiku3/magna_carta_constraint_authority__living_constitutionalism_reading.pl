% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_living_constitutionalism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Constraint Authority (Living Constitutionalism Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The living-constitutionalism reading of Magna Carta asserts that the
 *   charter establishes a juridical principle — that sovereign power is bound
 *   by inherited law and due process — that survives across centuries through
 *   continuous reinterpretation. The charter is not a static 13th-century
 *   feudal compact but a living standard that successive generations of
 *   lawyers, judges, and legislators elaborate to apply to new circumstances.
 *   Royal prerogative and executive discretion enter the victim set under
 *   this reading: the crown is constrained from above by the inherited
 *   charter. Subjects gain a shield: no arbitrary imprisonment, no property
 *   loss without lawful judgment. The constraint operates as genuine
 *   coordination (binding future sovereigns to a rule more predictable than
 *   their personal will) with low-to-moderate extractiveness because the
 *   charter benefits its subjects more than it extracts from the crown. The
 *   crown gains legitimacy in exchange for the constraint. This reading sits
 *   in direct tension with the feudal-obsolescence reading (which argues the
 *   charter is a historical artifact with no binding contemporary force) and
 *   influences but does not foreclose the parliamentary-sovereignty reading
 *   (which absorbs the charter into statute law and makes Parliament rather
 *   than inherited principle the locus of authority).
 *
 * KEY AGENTS:
 *   - subjects_with_due_process_rights: protected by the charter; gain a standard of lawful restraint that binds successive sovereigns — power: powerless; exit: trapped (bound to the realm, but the constraint protects them there)
 *   - crown_executive_prerogative: operates under inherited restraint; cannot arbitrarily dispose of property or imprison without lawful process — power: institutional; exit: constrained (inherits sovereignty bound, cannot escape the charter)
 *   - common_lawyers_custodians: interpret the charter's meaning across centuries; argue for evolutionary application to new grievances; gain professional standing and authority from custodianship — power: organized; exit: mobile (can leave the profession but benefit from staying)
 *   - parliamentary_legislative_bodies: inherit the charter's authority; elaborate due process protections into statute; benefit from the legitimacy the charter confers — power: institutional; exit: mobile (can revise or extend charter provisions within their authority)
 *   - feudal_baronial_class: negotiated the original charter; their grievances about arbitrary reliefs and wardships were the founding problem — power: powerful; exit: trapped (bound to the realm; their original interest in the charter is obscured by universalization)
 *   - rival_sovereigns_continental: observe whether the constraint creates institutional pressure for similar arrangements — power: institutional; exit: analytical (can compare models, choose whether to adopt restraint-models)
 *   - contemporary_constitutional_theorists: measure whether evolutionary interpretation preserves charter meaning or drifts into free construction — power: analytical; exit: analytical (observe, analyze, dispute)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.18).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Constraint Authority (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '639104be-7434-4286-9628-f6b3664d6249').
narrative_ontology:cs_kernel_codification('639104be-7434-4286-9628-f6b3664d6249', fixed_text).
narrative_ontology:cs_authority_grounding('639104be-7434-4286-9628-f6b3664d6249', lineage).
narrative_ontology:cs_interpretation_layer_present('639104be-7434-4286-9628-f6b3664d6249').
narrative_ontology:cs_reading_relation('639104be-7434-4286-9628-f6b3664d6249', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('639104be-7434-4286-9628-f6b3664d6249', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('639104be-7434-4286-9628-f6b3664d6249', foundational, inherited_charter_binds_successive_sovereigns).
narrative_ontology:cs_axiom_status(inherited_charter_binds_successive_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('639104be-7434-4286-9628-f6b3664d6249', inherited_charter_binds_successive_sovereigns, conventional).
narrative_ontology:cs_axiom('639104be-7434-4286-9628-f6b3664d6249', foundational, evolutionary_interpretation_preserves_principle_across_time).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_preserves_principle_across_time, holdable).
narrative_ontology:cs_axiom_grounding('639104be-7434-4286-9628-f6b3664d6249', evolutionary_interpretation_preserves_principle_across_time, deontological).
narrative_ontology:cs_reference_frame('639104be-7434-4286-9628-f6b3664d6249', inherited_charter_binding_sovereignty).
narrative_ontology:cs_drift_state('639104be-7434-4286-9628-f6b3664d6249', contemporary_security_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('639104be-7434-4286-9628-f6b3664d6249', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_with_due_process_rights).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, common_lawyers_custodians).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_legislative_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a shield against arbitrary executive action: no fine, imprisonment, or loss of property except by lawful judgment. The constraint's operation creates an appeal to a standard (the inherited charter) that binds even the sovereign. Their exit is none — they are bound to the realm — but their position is protected by the constraint's juridical force.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_with_due_process_rights, beneficiary,
    powerless, generational, trapped, national).

% Operates under inherited restraint: cannot dispose of property arbitrarily, cannot imprison without lawful process, cannot suspend the laws except in extremity and only via recorded procedure. The constraint's persistence creates a check on executive discretion that successive monarchs must formally acknowledge. They cannot exit — they inherit sovereignty bound by the charter — but their power is structurally constrained.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive_prerogative, payer,
    institutional, generational, constrained, national).

% Serve as the active interpreters and custodians of the charter's meaning across centuries. They argue for continuity of principle through changing circumstance ('evolutionary interpretation'), defend the charter's binding force against claims of obsolescence, and develop doctrine that makes the inherited text speak to new grievances. They benefit from the authority the charter grants them and from the professional standing that interpretation confers.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_lawyers_custodians, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, common_lawyers_custodians, beneficiary).

% Inherit the charter's authority in this reading; Parliament becomes the forum where due process protections are elaborated into statute. The constraint benefits Parliament by grounding legislative supremacy in an ancient principle. Parliament's capacity to interpret and extend the charter's reach makes it both beneficiary (of the legitimacy it inherits) and partial co-setter of the constraint's contemporary meaning.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_legislative_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_legislative_bodies, agenda_setter).

% In the 13th century moment of creation, held grievances about arbitrary reliefs, wardships, and arbitrary forfeiture. The constraint was originally negotiated as a baronial compact. In the living-constitutionalism reading, their original grievances are reframed as universal principles, but the baronial interest in the constraint's original negotiation is obscured by universalization.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_baronial_class, excluded,
    powerful, biographical, trapped, national).

% Observe whether England's experiment in binding sovereignty to inherited law creates institutional pressure for similar arrangements. The constraint's existence and asserted legitimacy create a regional-influence fact: other realms must either adopt the restraint-model or explicitly reject it, making their prerogative appear less restrained by comparison.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, rival_sovereigns_continental, observer,
    institutional, generational, analytical, global).

% Analyze whether the constraint's claimed binding force across centuries is structurally sound. They witness the constraint's operation through precedent and statutory elaboration and measure whether evolutionary interpretation preserves the charter's meaning or drifts into free construction.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, contemporary_constitutional_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, crown_executive_prerogative).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a lasting solution to recurring executive overreach: rather than fight the prerogative anew in each reign, subjects and barons agree that any sovereign inherits a charter that binds them to lawful action. The constraint solves the coordination problem of how to make prerogative power habitual and predictable instead of capricious.
% TRANSFER_FUNCTION: Moves legitimacy from the monarch's personal will to the inherited law: the sovereign gains a constraint it must observe, but in exchange gains the legitimacy of lawfulness and continuity. Subjects gain a standard they can appeal to; the charter is what transfers authority from brute prerogative to juridical restraint.
% ABSENT_VOICES: Feudal peasants and serfs are excluded entirely — the charter addresses baronial and subject grievances at the level of freemen. The constraint's scope is written to subjects of substantial property who could be subject to arbitrary reliefs and forfeiture. Serfs and unfree persons had no standing to invoke the charter's protection; their exclusion is structural, not accidental.
% DISAPPEARANCE_RATIONALE: If the inherited constraint vanished — if the charter were repudiated and no principle bound successive sovereigns — executive prerogative would revert to capriciousness. Subjects would lose the standard they could appeal to in court. The entire institutional framework of England's common law, which rests on the assumption that the ruler is bound by inherited law, would require reorganization. Parliamentary supremacy, which grounds itself partly in the charter's authority, would lose a foundational legitimacy claim.
% FOUNDING_PROBLEM: Arbitrary executive action under feudal tenure: kings disposed of reliefs, wardships, and escheats without lawful restraint; barons and freemen faced loss of property and liberty without legal process; each reign brought renewed negotiation of the same grievances, with no permanent solution.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers and baronial records of the 13th century attest the founding problem. In the living-constitutionalism reading, common lawyers and constitutional theorists from the 16th century onward (Coke, Blackstone) attest that the founding problem remains live: executive prerogative is a perpetual danger that the inherited charter restrains. However, legal historians outside the common-law tradition (including some modern scholars) attest that the founding problem is historically specific to medieval feudal tenure and that the charter's binding authority beyond its original context is asserted rather than structural.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness declines sharply from the constraint's origin (0.35 in 1215) to its nadir (0.10 in 1950), tracking the degree to which executive prerogative is genuinely restrained. In 1215, the constraint is novel and contested; the crown extractiveness is high because the charter must be enforced against sovereign resistance. As the charter becomes embedded in common law doctrine (by 1600, Coke's reinterpretation makes it a binding principle of the English constitution), extractiveness drops because the constraint operates through juridical continuity, not active negotiation. The floor at 1950 reflects full internalization: by the mid-20th century, the principle that executive action requires lawful authority is so embedded that the constraint requires no suppressive enforcement. The rise to 0.18 in 2024 reflects contemporary contestation around executive emergency powers, surveillance authority, and the scope of due process — the constraint's meaning is live again as new technologies and security doctrines strain the inherited principle. Suppression follows a similar arc: high in the medieval period when the crown must actively resist the charter, declining as the principle becomes habitual, rising again as modern expansions of executive power require the constraint to be actively reasserted in courts. Theater is low throughout (never above 0.12) because the constraint's enforcement is genuinely juridical, not performative — judges apply the charter to cases; lawyers elaborate its meaning through precedent. The brief rise to 0.12 in 2024 reflects performative invocation of 'constitutional tradition' in political speech around executive overreach, without corresponding constraint on actual power.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (crown) and the beneficiary seats (subjects, lawyers) compute different types from the same structural data. From the crown's seat, the constraint is a structural feature of legitimate sovereignty (rope: coordination around inherited restraint, with the crown's legitimacy as the coordinating benefit). From the subjects' seat, the constraint is a protection against extraction (the crown would extract arbitrarily without the charter, so the constraint prevents that extraction — still rope, but with protection as the benefit rather than legitimacy as the coordination). From the contemporary seat, in 2024, the constraint is increasingly contested: executive prerogatives in emergency, surveillance, and security contexts strain the inherited principle, and the constraint's type begins to shift toward tangled_rope in those domains (coordination function decays; extraction persists). The engine computes this seat-by-seat type divergence; the authored claim (rope) reflects the constraint's structural type when it is functioning as designed.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown's directionality (d near 0.35-0.40 range, approaching target end) reflects that the constraint extracts from executive prerogative: the crown cannot act arbitrarily, must follow lawful procedure, and this obligation persists across reigns. However, the crown's power is institutional, giving it some capacity to contest the constraint's scope — hence d does not reach the full-target range (0.70+). Subjects' directionality (d near 0.15-0.25, near beneficiary end) reflects that they benefit from the constraint far more than they bear costs — they gain protection without having to enforce it themselves (the lawyers and judges do that). Common lawyers (d near 0.20-0.30) sit near beneficiary because they gain professional standing and authority from custodianship, though they incur the cost of constantly defending the constraint against claims of obsolescence. Parliament (d near 0.25-0.35) is more centered because it both benefits from inherited legitimacy and bears the constraint on its own power to act — Parliament can elaborate the charter but cannot simply repeal it without invoking the parliamentary-sovereignty reading (a different reading, a different constraint). No directionality overrides are required: the structural derivation from beneficiary/victim + power + exit produces accurate d values across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy precisely because the founding problem (arbitrary executive action) remains live and the constraint's function (binding successive sovereigns to inherited law) continues to perform. The measurement series shows that extractiveness and suppression do not accumulate monotonically; instead, they cycle with the constraint's visibility. When executive power attempts to expand (1215, 2024), the constraint becomes active and suppression rises. When executive power is restrained by other institutions (1950), the constraint's suppression floor drops. This cyclical pattern is diagnostic of a rope or tangled_rope, not a piton: the constraint is not merely theatrical; it is called upon and genuinely constrains when tested. The theater_ratio stays low (max 0.12), indicating that the constraint's enforcement is substantive. However, the rise in both extractiveness and suppression from 2024 onward suggests future mandatrophy risk: if emergency powers and surveillance permanently shift the executive's effective scope, and if the inherited constraint becomes merely invoked (in political speech) but not actually enforced (in courts and practice), the constraint could degrade into piton (still cited, still verbally binding, but increasingly theatrical). An omega variable addresses this trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_interpretation_boundary,
    'Where is the boundary between legitimate evolutionary interpretation of the charter''s meaning and free construction that treats the inherited text as mere advisory?',
    'Comparative institutional analysis: when courts invoke the charter to strike down executive action, do they ground the decision in principles traceable to the charter''s text and historical application, or do they read the charter as simply validating contemporary due-process norms? If the latter, the charter is no longer binding inherited law but a legitimacy label for independently derived constitutional law.',
    'If the boundary erodes and evolutionary interpretation becomes free construction, the constraint transitions from rope (binding inherited principle) to tangled_rope (the crown is coordinated by a principle it no longer truly inherits) or even snare (the constraint persists as legitimacy theater for courts'' actual policy choices). The type computation would shift downward; mandatrophy risk rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_interpretation_boundary, conceptual, 'Whether inherited-law interpretation remains tethered to the charter''s meaning or drifts into independent constitution-making.').

omega_variable(
    emergency_power_exception,
    'Does the inherited constraint on executive prerogative survive in emergency and national security contexts, or does emergency logic create a shadow executive realm outside the charter''s restraint?',
    'Institutional audit: trace judicial enforcement of due-process constraints in national-security and emergency cases from the 1940s onward. If restraint persists (courts enforce habeas corpus even in war, executive action is reviewed), the constraint is intact; if courts defer entirely to executive necessity claims, the constraint has eroded into a narrower, vestigial form.',
    'If emergency exceptions swallow a substantial domain of executive power, the effective scope of the constraint shrinks even if it persists textually. Extractiveness could rise in the security domain while remaining low elsewhere, suggesting domain-specific mandatrophy. The constraint type might remain rope in civil domains while degrading to piton in security domains — a jurisdiction-split result that the engine would compute as seat-specific rather than uniform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_power_exception, empirical, 'Whether the inherited constraint applies across all executive domains or cedes emergency and security to unrestrained prerogative.').

omega_variable(
    feudal_vs_universal_reading_ambiguity,
    'Are the charter''s restraints a universalizable principle of due process, or are they feudal-context-specific protections that lost their original meaning when feudal tenure ended?',
    'Historical analysis: do the specific grievances in the charter (arbitrary reliefs, wardships, escheats without process) remain intelligible as instances of a general principle (no property loss without lawful judgment) that applies to modern contexts, or are they historically contingent? Legal historians outside the common-law tradition attest feudal-obsolescence; common lawyers and constitutional judges attest universalization.',
    'If the feudal-obsolescence reading is correct, the constraint is a piton maintained by institutional inertia and the authority claims of common lawyers. If the universalization reading is correct, the constraint is a genuine rope that binds across centuries. The type computation depends on this axiom: if the charter''s meaning is universal, extractiveness remains low and suppression stays structural; if the charter is feudal-specific, extractiveness would be recomputed upward (the constraint persists not because it benefits subjects or restrains the crown, but because inherited authority benefits lawyers and judges) and the type would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feudal_vs_universal_reading_ambiguity, conceptual, 'Whether Magna Carta establishes a universal principle of due process or a feudal-context-specific protective compact.').

omega_variable(
    kernel_reading_committer_structure,
    'Does this constraint instantiate the living-constitutionalism reading, or does authoring it as a clean ε-invariant constraint collapse the reading''s contested nature into an uncontested claim?',
    'Meta-level: this constraint is authored as ONE reading of a kernel. The feudal-obsolescence and parliamentary-sovereignty readings are OTHER constraints, not frames within this one. If the corpus generates those sibling readings and they exhibit substantially different ε values and type classifications, the framework validates that decomposition. If they do not, the kernel contest is not real — the readings are façade variations on a single constraint.',
    'This omega documents the generative assumption: Magna Carta is NOT a single constraint viewed from three angles. It is three structurally distinct constraints arising from three different readings of what the charter IS and what it BINDS. The ε-invariance principle requires decomposition; this omega flags that the decomposition is structurally sound and not just terminological hairsplitting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, preference, 'Whether Magna Carta constraint authority is meaningfully decomposed into three reading-specific constraint stories or collapsed into one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement(magn_tr_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1350, 0.1).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1688, 0.06).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1832, 0.08).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magn_be_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1350, 0.28).
narrative_ontology:measurement(magn_be_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1600, 0.22).
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1688, 0.15).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1832, 0.12).
narrative_ontology:measurement(magn_be_t1950, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.45).
narrative_ontology:measurement(magn_su_t1350, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1350, 0.38).
narrative_ontology:measurement(magn_su_t1600, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1600, 0.28).
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1688, 0.15).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1832, 0.12).
narrative_ontology:measurement(magn_su_t1950, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta kernel. The feudal-obsolescence reading interprets the charter as a historical artifact whose meaning is bound to 13th-century feudal context and has no contemporary binding force — ε would be high (the charter's invocation is cover for independently chosen restraint). The parliamentary-sovereignty reading interprets the charter as valid law only insofar as Parliament has absorbed it into statute — ε would be moderate (Parliament's sovereignty is the locus of restraint, not the charter itself). The living-constitutionalism reading (this story) interprets the charter as establishing a principle (inherited due process) that survives through reinterpretation — ε is low (genuine coordination around inherited restraint). These three readings have structurally distinct ε values and victim sets (in feudal-obsolescence, nobody is truly a victim because the constraint is fictional; in parliamentary-sovereignty, both crown and Parliament are constrained by Parliament's need to uphold statute; in living-constitutionalism, the crown is constrained by inherited law). The ε-invariance principle requires three separate constraint stories linked by network.affects_constraints. Empirical validation: if the three stories show convergent type classifications despite different ε values, the kernel contest is real. If they collapse to the same type, the decomposition was notational only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
