% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: 1890 Manifesto as Temporal Accommodation of Plural Marriage Doctrine
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto declared an end to the sanctioning of new plural
 *   marriages by the Church of Jesus Christ of Latter-day Saints under
 *   federal legal pressure. Rather than declaring the underlying 1843
 *   revelation (D&C 132) false or rescinded, the Manifesto and subsequent
 *   leadership statements were carefully worded to suspend practice while
 *   leaving the doctrine's eternal validity formally untouched. This reading
 *   treats the arrangement as a hybrid: a genuine institutional coordination
 *   problem (survive federal prosecution, secure statehood) solved through a
 *   structure that also extracted real costs from existing plural families,
 *   dissenting literalists, and especially plural wives left in unresolved
 *   legal and social status. This is one of three readings of the same kernel
 *   (the eternal marriage covenant and its relationship to the 1890
 *   Manifesto); the other two — that the doctrine is immutable divine law
 *   requiring polygamy for exaltation, and that continuing revelation
 *   empowers the living prophet to fully supersede prior revelation — are
 *   separate constraints with separate ε values, linked here structurally but
 *   not merged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.58).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.62).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "1890 Manifesto as Temporal Accommodation of Plural Marriage Doctrine").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, '7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc').
narrative_ontology:cs_kernel_codification('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', formalized).
narrative_ontology:cs_authority_grounding('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', lineage).
narrative_ontology:cs_interpretation_layer_present('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc').
narrative_ontology:cs_reading_relation('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', eternal_marriage_covenant__prophetic_override_reading, influences).
narrative_ontology:cs_axiom('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', foundational, doctrine_and_practice_are_separable).
narrative_ontology:cs_axiom_status(doctrine_and_practice_are_separable, holdable).
narrative_ontology:cs_axiom_grounding('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', doctrine_and_practice_are_separable, conventional).
narrative_ontology:cs_axiom('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', foundational, civil_obedience_takes_precedence_over_practice_of_valid_principle).
narrative_ontology:cs_axiom_status(civil_obedience_takes_precedence_over_practice_of_valid_principle, holdable).
narrative_ontology:cs_axiom_grounding('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', civil_obedience_takes_precedence_over_practice_of_valid_principle, instrumental).
narrative_ontology:cs_axiom('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', secondary, suspension_is_temporally_bounded_pending_restoration).
narrative_ontology:cs_axiom_status(suspension_is_temporally_bounded_pending_restoration, overridden).
narrative_ontology:cs_axiom_grounding('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', suspension_is_temporally_bounded_pending_restoration, empirically_contingent).
narrative_ontology:cs_reference_frame('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', id_1843_revelation_as_standing_eternal_law).
narrative_ontology:cs_drift_state('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', post_manifesto_statehood_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7edab731-5496-4cd6-b5ea-9cdb8e9d8dbc', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, statehood_seeking_utah_political_class).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, post_manifesto_plural_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, women_in_dormant_plural_unions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 Manifesto declaring intent to submit to federal anti-polygamy law while carefully avoiding language that repudiates the underlying doctrine as false. Negotiates directly with federal officials over amnesty, restored property, and a path to statehood. Retains the institutional authority to later reinterpret, extend, or fully abandon the suspension, and controls which members are disciplined for continuing the practice versus quietly tolerated.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, beneficiary).

% Territorial politicians and church-aligned business interests need federal recognition and statehood to secure property rights, congressional representation, and economic normalization. They benefit directly from the Manifesto's ambiguity: it satisfies federal demands on paper while preserving the social capital of a doctrine many still privately hold as true.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, statehood_seeking_utah_political_class, beneficiary,
    organized, biographical, constrained, national).

% Existing plural households formed before 1890 are told the practice is suspended but not that their marriages are dissolved or their doctrine false. They live in a legal and social limbo: unable to formalize their unions publicly, subject to prosecution risk if discovered, and denied a clear institutional answer about whether their family structure is currently sanctioned, tolerated, or condemned.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, post_manifesto_plural_families, payer,
    powerless, biographical, trapped, regional).

% Members who take the doctrine's continued validity literally continue or begin new plural marriages, reasoning that the Manifesto only addresses political obedience, not eternal truth. They are excommunicated and denounced by the same institution whose own manifesto language preserved the doctrine's validity, absorbing the full cost of a distinction the institution created but does not itself have to live inside.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, fundamentalist_dissenters, payer,
    moderate, generational, constrained, regional).

% Wives in second or subsequent plural marriages bear the direct social and legal weight of the suspension: no public legitimacy for their marriage, no inheritance clarity, exposure to prosecution or shaming, and no institutional voice in whether the suspension is temporary or permanent. Their consent was structured around a doctrine now administratively frozen without their input.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, women_in_dormant_plural_unions, payer,
    powerless, biographical, trapped, local).

% Demands cessation of the practice as a condition for ending prosecutions, seizure of church property, and blocking statehood. Reads the Manifesto as a claimed full renunciation and later population registers this ambiguity when covert plural marriages continue after 1890, but has no seat inside the doctrinal reasoning that preserves the principle as eternally valid while suspending its practice.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Study the gap between the Manifesto's public presentation as a doctrinal end and the private continuation of plural marriages authorized by some church leaders into the early 20th century (documented in the Second Manifesto of 1904), providing the historical record from which the temporal-accommodation reading is later reconstructed and contested.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a face-saving mechanism allowing the institution to comply with federal law, end escalating prosecutions and property seizures, and secure a path to statehood, without having to declare its founding revelation false — a genuine coordination problem between institutional survival and doctrinal continuity.
% TRANSFER_FUNCTION: Moves legal risk and social stigma from the institution (which negotiates amnesty and normalization) onto individual plural families and especially plural wives, who absorb the ambiguity of a doctrine declared eternally true but currently impermissible to practice, with no clear institutional protection or resolution.
% ABSENT_VOICES: Women in plural marriages had no formal voice in either the original doctrine's promulgation or the Manifesto's drafting; fundamentalist dissenters who took the 'eternal principle' language literally were later cast out rather than consulted about the ambiguity the leadership itself authored.
% DISAPPEARANCE_RATIONALE: If the temporal-accommodation framing (suspend-without-renounce) had not existed — if the Manifesto had either fully renounced the doctrine or fully maintained it without qualification — the institution's subsequent history would differ sharply: either no fundamentalist schism claiming doctrinal continuity, or no path to federal accommodation and statehood. The ambiguity itself is load-bearing for both outcomes that followed.
% FOUNDING_PROBLEM: Federal anti-bigamy prosecutions (Edmunds Act, Edmunds-Tucker Act), disincorporation of the church, and seizure of church property created an existential institutional crisis that plural marriage practice could not survive unaltered.
% FOUNDING_PROBLEM_CORROBORATION: Federal prosecutors and courts of the 1880s-90s (outside the church) attested the practice had in fact ceased to the degree required for property restoration and statehood negotiations; independent historians documented continued authorized plural marriages after 1890 (leading to the 1904 Second Manifesto), corroborating from outside the church's own beneficiary narrative that the original suspension was incomplete and the 'eternal principle' framing was doing real institutional work rather than being a mere formality.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at steady state) because the arrangement transfers real legal and social risk onto individuals who continued or entered plural marriages believing the doctrine, and onto existing plural wives who had no institutional path to clarity. Theater ratio rises over the interval (0.35 to 0.55) reflecting the growing gap between the public 'the practice has ended' narrative and the documented continuation of authorized plural marriages into the early 1900s, culminating in the need for a Second Manifesto in 1904 — a classic Goodhart-drift signature where the proxy (public declaration) diverges from the underlying reality (continued sanctioning) until forced correction. Suppression is high initially (0.70) reflecting active federal and institutional pressure, declining slightly as normalization set in.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits at the beneficiary end: it negotiates the accommodation, retains authority over doctrinal interpretation, and secures institutional survival and statehood benefits, all while bearing none of the individual-level legal exposure. Post-Manifesto plural families and especially women in dormant plural unions sit at the target end: trapped exit options, no voice in the ambiguous framing, and direct exposure to prosecution or social stigma from a status the institution itself declined to resolve cleanly. Fundamentalist dissenters occupy an intermediate but costly position — they take the preserved 'eternal principle' language at face value (as the institution's own text invites) and are then punished for doing so, which is the structural signature of a tangled rope rather than a clean rope: the coordination function (institutional survival) is real, but it is achieved by transferring an unresolved cost onto parties who had no say in how the ambiguity would later be adjudicated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal prosecution threat) is dead by 1930 — Utah achieved statehood in 1896 and prosecutions largely ceased — yet the doctrinal ambiguity persisted for decades (formally addressed again only by the 1904 Second Manifesto and later disciplinary actions against continuing practitioners into the 1930s-40s). This is exactly the mandatrophy pattern the classification exists to catch: a suspension justified by an acute crisis outliving that crisis by generations, with the 'eternal principle remains valid' language doing ongoing institutional work (preserving continuity with founding leaders, deflecting the 'the doctrine was simply wrong' conclusion) long after the political emergency that justified the suspension had resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_duration_indeterminacy,
    'Was the suspension ever intended to be genuinely temporary (pending a future lifting of political constraints), or was ''eternal principle remains valid'' rhetorical cover for what leadership understood internally to be a permanent doctrinal retreat?',
    'Private correspondence and journals of church leadership from 1890-1904 (some since published) documenting internal deliberation about whether plural marriage would ever resume; comparison with the language and enforcement pattern of the 1904 Second Manifesto, which moved toward permanent excommunication for continued practice.',
    'If genuinely intended as temporary, the arrangement is better read as scaffold-adjacent (transitional coordination with an implicit, if unstated, sunset expectation). If understood internally as permanent from the start, the ''eternal principle'' language is closer to pure theater serving continuity-of-authority functions, pushing the classification toward tangled_rope with a higher theater component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_duration_indeterminacy, empirical, 'Whether the suspension was authored as genuinely temporary or as permanent retreat under a temporary label.').

omega_variable(
    kernel_framing_alternative_authority_text,
    'Is the correct kernel-reading framing ''the Manifesto text itself'' (the obvious framing used here) or ''the retrospective institutional narrative of the Manifesto''s meaning constructed across the 1904 Second Manifesto and 20th-century leadership statements'' (the less obvious framing, since the 1890 text is famously terse and its doctrinal meaning was substantially filled in by later interpretive acts)?',
    'Textual and historical analysis comparing the 1890 document''s actual language against the interpretive gloss added by Wilford Woodruff''s later personal statements, the 1904 Second Manifesto, and 20th-century official Church histories, to determine how much of the ''eternal principle preserved'' reading is in the 1890 text versus retroactively constructed.',
    'If the temporal-accommodation reading is substantially a retrospective construction rather than present in the 1890 text, this constraint''s cs_structure.kernel_codification should arguably be ''distributed'' or ''implicit'' rather than ''formalized'' — the kernel would be under-specified at origin and stabilized only through later interpretive layering, which would also affect how firmly the axioms below can be attributed to the founding moment versus a later apologetic tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_authority_text, conceptual, 'Whether the kernel text or its later interpretive gloss is the true referent of this reading.').

omega_variable(
    beneficiary_vs_genuine_crisis_response,
    'Is church institutional leadership best modeled as a beneficiary extracting institutional survival value from an ambiguous arrangement imposed on individuals, or as an agent genuinely trapped between two bad options (full doctrinal renunciation versus institutional destruction) with no clean exit of its own?',
    'Comparative analysis of how much discretionary latitude leadership actually had in 1890 (e.g., whether milder compliance postures were realistically available) versus how much of the ambiguous framing was strategically chosen beyond what compliance strictly required.',
    'If leadership had little real discretion, its ''beneficiary'' role is partly an artifact of the analysis rather than genuine extraction, and the arrangement leans closer to rope (coordinated survival response) despite the individual-level costs. If leadership had substantial discretion and chose ambiguity for institutional-continuity advantage beyond what compliance required, the tangled_rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_genuine_crisis_response, conceptual, 'Whether institutional leadership''s beneficiary position reflects genuine extraction or unavoidable crisis navigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(eter_tr_t1898, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1898, 0.45).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1904, 0.5).
narrative_ontology:measurement(eter_tr_t1912, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1912, 0.55).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.55).
narrative_ontology:measurement(eter_tr_t1930, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1930, 0.55).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement(eter_be_t1898, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1898, 0.5).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1904, 0.55).
narrative_ontology:measurement(eter_be_t1912, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1912, 0.58).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(eter_be_t1930, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1930, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement(eter_su_t1898, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1898, 0.68).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement(eter_su_t1912, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1912, 0.62).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.62).
narrative_ontology:measurement(eter_su_t1930, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1930, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__temporal_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the eternal_marriage_covenant kernel. immutable_commandment_reading treats D&C 132 as unconditionally binding, making the Manifesto itself the extractive imposition (victims: believers coerced into abandoning a commanded practice). prophetic_override_reading treats continuing revelation as fully and legitimately superseding the prior revelation, closing the doctrinal question (much lower ε, near-mountain: the override is treated as a settled feature of the authority structure rather than a contested accommodation). This reading (temporal_accommodation_reading) sits structurally between them: doctrine neither commanded-and-violated nor cleanly superseded, but suspended-and-dormant, which generates its own distinct victim set (families and dissenters caught in the resulting ambiguity) not present in the same form in either sibling reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
