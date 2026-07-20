% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Divine-Right Legitimacy (Sovereign Legitimacy Kernel â Monarchical Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the monarchical reading of the
 *   sovereign_legitimacy kernel: the claim that legitimate political
 *   authority descends from a sovereign to subjects through inherited right,
 *   sanctified by divine will, tradition, and bloodline continuity. It is
 *   structurally contested by the republican reading (authority ascends from
 *   the people) and the constitutional hybrid reading (dual-sourced authority
 *   with a mediated boundary). The constraint solves a genuine coordination
 *   problemâclear succession rules reduce violent leadership
 *   contestsâwhile asymmetrically extracting authority, surplus, and status
 *   from the subject population to a hereditary class. Active enforcement is
 *   required to suppress alternative legitimacy claims and maintain the
 *   bloodline monopoly on supreme office.
 *
 * KEY AGENTS:
 *   - Hereditary sovereign (institutional/identity_locked): apex agenda-setter whose personal identity is fused with the office; primary beneficiary of extracted authority and deference.
 *   - Aristocratic hierarchy (powerful/constrained): secondary beneficiaries who gain ranked privilege from the hierarchical order but do not set legitimacy rules.
 *   - Clerical legitimators (organized/constrained): beneficiaries who supply theological validation and receive patronage; their survival is contingent on the monarchy's persistence.
 *   - Excluded subjects (powerless/trapped): bear taxes, service, and deference; have no voice in ruler selection and face barred exit.
 *   - Republican dissidents (moderate/trapped): excluded voices advocating popular sovereignty; suppressed as treasonous.
 *   - Political theorist observer (analytical/analytical): external analytical seat comparing legitimacy frameworks without bearing the constraint's costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.86).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.91).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Divine-Right Legitimacy (Sovereign Legitimacy Kernel â Monarchical Reading)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '3b00ed36-bb86-4683-8e42-e8eb60260373').
narrative_ontology:cs_kernel_codification('3b00ed36-bb86-4683-8e42-e8eb60260373', fixed_text).
narrative_ontology:cs_authority_grounding('3b00ed36-bb86-4683-8e42-e8eb60260373', lineage).
narrative_ontology:cs_interpretation_layer_present('3b00ed36-bb86-4683-8e42-e8eb60260373').
narrative_ontology:cs_reading_relation('3b00ed36-bb86-4683-8e42-e8eb60260373', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b00ed36-bb86-4683-8e42-e8eb60260373', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('3b00ed36-bb86-4683-8e42-e8eb60260373', foundational, divine_bloodline_authority).
narrative_ontology:cs_axiom_status(divine_bloodline_authority, holdable).
narrative_ontology:cs_axiom_grounding('3b00ed36-bb86-4683-8e42-e8eb60260373', divine_bloodline_authority, theological).
narrative_ontology:cs_axiom('3b00ed36-bb86-4683-8e42-e8eb60260373', foundational, hereditary_succession_legitimacy).
narrative_ontology:cs_axiom_status(hereditary_succession_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3b00ed36-bb86-4683-8e42-e8eb60260373', hereditary_succession_legitimacy, conventional).
narrative_ontology:cs_reference_frame('3b00ed36-bb86-4683-8e42-e8eb60260373', divine_bloodline_continuity).
narrative_ontology:cs_drift_state('3b00ed36-bb86-4683-8e42-e8eb60260373', contemporary_liberal_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3b00ed36-bb86-4683-8e42-e8eb60260373', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_sovereign).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, clerical_legitimators).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, republican_dissidents).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, bloodline_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the apex of authority by virtue of birth order within a designated bloodline. Claims and receives obedience, taxation, and deference from subjects. Maintains the constraint through court ritual, patronage networks, and enforcement of treason and lÃ¨se-majestÃ© laws. Abdication is structurally possible but historically rare and socially costly; personal identity is fused with the office.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_sovereign, agenda_setter,
    institutional, generational, identity_locked, national).

% Holds privileged status, heritable titles, land, and exemptions granted or recognized by the sovereign. Benefits from the hierarchical social order that locates them permanently above common subjects. Their position depends on monarchical recognition and confirmation; they may advise the sovereign but do not set the fundamental legitimacy rules.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy, beneficiary,
    powerful, generational, constrained, national).

% Interpret sacred texts, maintain genealogical records, and perform rituals that confirm the sovereign's divine selection and traditional right. Receive institutional patronage, land endowments, and social standing from the monarchical system in exchange for theological validation. Their authority and material security are contingent on the monarchy's survival.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, clerical_legitimators, beneficiary,
    organized, generational, constrained, national).

% Compose the majority of the population. Owe taxes, military service, corvÃ©e labor, and deference to the sovereign and aristocracy. Hold no formal role in selecting, checking, or removing rulers. Geographic exit is often legally barred; voice is limited to petition or revolt, both punished as disorder.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_subjects, payer,
    powerless, biographical, trapped, national).

% Advocate that legitimate authority must derive from popular consent or social contract rather than bloodline. Are formally excluded from political participation and often subject to imprisonment, exile, or execution. Their claims are categorized as treason or heresy by the prevailing legitimacy framework, not as contestable political positions.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_dissidents, excluded,
    moderate, biographical, trapped, national).

% Studies the structure of legitimacy claims from outside the system. Neither pays the constraint nor benefits from its operation. Can compare monarchical, republican, and hybrid frameworks comparatively without facing the suppression borne by internal dissidents.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, political_theorist_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of leadership succession by predetermining supreme authority through bloodline continuity and ritual confirmation, eliminating open contests for power and providing a single acknowledged decision-maker to settle disputes and command allegiance.
% TRANSFER_FUNCTION: Transfers authority, material surplus, and social status from the subject population to the hereditary sovereign and aristocratic hierarchy; transfers ideological compliance and deference from subjects to the divine-right narrative maintained by clerical legitimators.
% ABSENT_VOICES: Republican dissidents advocating popular sovereignty, constitutional reformists seeking power-sharing arrangements, and subjects who reject bloodline status as a basis for political obligation are structurally excluded from legitimacy discourse; their voices are suppressed as treasonous, heretical, or seditious rather than engaged as political alternatives.
% DISAPPEARANCE_RATIONALE: If the monarchical legitimacy constraint vanished overnight, succession would become openly contested by multiple claimants and factions, likely triggering civil conflict; the social hierarchy organized around bloodline privilege and divine sanction would lose its primary justification and would either collapse or forcibly reconstitute around alternative legitimacy principles such as popular sovereignty, military command, or theocratic council.
% FOUNDING_PROBLEM: The recurrent violence, instability, and civil war of contested leadership transitions in polities lacking a clear, accepted succession rule, where armed kinship groups, warlords, or factions fought for supreme authority after the death or incapacity of a leader.
% FOUNDING_PROBLEM_CORROBORATION: Political historians outside the beneficiary classes corroborate that hereditary rules reduced certain succession wars in specific medieval and early modern contexts; democratic transition scholars and republican theorists attest that peaceful power transfer is now better achieved through electoral, constitutional, and deliberative mechanisms, corroborating that alternative coordination methods exist and the founding problem's current status is disputed.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.86, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.86) because the constraint concentrates supreme authority, material surplus, and social status in a narrow hereditary class, decoupled from merit or consent. Suppression is higher still (0.91) because the constraint's persistence depends on actively excluding republican and hybrid alternatives, criminalizing dissent, and controlling succession narratives. Accessibility collapse is high (0.88): once the divine-right and bloodline framework is accepted, alternative legitimacy principles appear unthinkable or wicked. Resistance is substantial (0.72) because republican and reformist movements recurrently challenge the framework, though they are often crushed. Theater ratio rises over the interval (0.30â0.62) as the monarchy increasingly maintains power through ritual performance and symbolic continuity while actual governance shifts to bureaucratic or delegated bodies.
 *
 * PERSPECTIVAL GAP:
 *   The hereditary sovereign and aristocratic beneficiaries experience the constraint as natural order, sacred duty, and necessary social stability; the engine should compute their seats as low-directionality beneficiaries. Excluded subjects and republican dissidents experience the same structure as arbitrary domination enforced by violence and ideology; the engine should compute their seats as high-directionality targets. The clerical legitimators sit in betweenâthey benefit materially but are constrained by dependence, yielding a mid-range directionality. The magnitude of seat divergence is large because the constraint fuses identity (bloodline, divine selection) with power, making exit impossible for targets and unthinkable for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality downward for the hereditary sovereign, aristocratic hierarchy, and clerical legitimators: they collect authority, surplus, and status from the constraint's operation. Victim declarations drive directionality upward for excluded subjects and republican dissidents: they bear the costs of extraction and suppression. The sovereign's identity_locked exit amplifies their beneficiary position because they cannot leave the role without social death. Subjects' trapped exit amplifies their target position because they cannot geographically or politically exit the scope. The theorist observer has analytical exit and receives neither benefits nor costs, sitting near d=0.5 by structural default.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by preserving its genuine coordination function: hereditary succession historically reduced certain succession wars by providing a clear, precommitted rule. However, the high suppression and extractiveness metrics prevent mislabeling it as mere rope by documenting that the coordination function is asymmetrically captured and actively enforced against alternatives. If the coordination function atrophied entirelyâsuccession became purely performative while real power moved elsewhereâthe drift path would point toward piton; the rising theater_ratio series tracks this possibility without prejudging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_epistemic_type,
    'Does the divine sanction grounding function as an empirically testable claim (the monarch is literally chosen by a detectable supernatural power) or as a conventional or theological commitment that is not empirically falsifiable?',
    'Historical and anthropological analysis of how legitimacy brokers frame divine selectionâthrough oracles, priestly confirmation, miraculous validation, or social conventionâand whether these framings are treated as open to empirical test within the tradition.',
    'If purely conventional or theological, the constraint''s accessibility_collapse is socially constructed rather than metaphysically necessary; if treated as empirical, alternatives are perceived as literally impossible, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_epistemic_type, conceptual, 'Epistemic status of divine sanction grounding').

omega_variable(
    coordination_extraction_boundary,
    'Does the hereditary succession rule primarily coordinate (prevent civil war over leadership) or primarily extract (concentrate power in a bloodline class), and is the coordination separable from the extraction?',
    'Comparative historical analysis of polities that achieved stable succession through non-hereditary means (elective monarchy, republican transition, oligarchic rotation) to test whether the coordination benefit requires bloodline concentration.',
    'If coordination is separable from bloodline extraction, the constraint is extractive cover; if inseparable, a larger share of measured extraction may be inherent coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Separability of succession coordination from bloodline extraction').

omega_variable(
    kernel_reading_scope,
    'This constraint is one reading of the sovereign_legitimacy kernel. How would structural classification change if the republican or constitutional hybrid reading were adopted instead?',
    'Comparison of sibling constraint stories within the same kernel to identify which structural elements (beneficiary sets, suppression levels, theater ratios) shift across readings.',
    'If sibling readings produce materially different epsilon values and directionality distributions, the kernel is confirmed as a genuine decomposition case; if readings converge, the kernel may be a single constraint with superficial framing variance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Structural variance across kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, physical barriers, institutional exclusion) or internalized (subjects believe bloodline authority is natural, divinely ordained, or morally required)?',
    'Post-revolutionary or post-reform trajectory analysis: if resistance and rejection of monarchical legitimacy surge rapidly after structural enforcement is removed, suppression was substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure because subjects carry the constraint after external barriers fall; if purely structural, removal should produce immediate reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovleg_mon_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sovleg_mon_tr_t10, sovereign_legitimacy__monarchical_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(sovleg_mon_tr_t20, sovereign_legitimacy__monarchical_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sovleg_mon_tr_t30, sovereign_legitimacy__monarchical_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(sovleg_mon_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(sovleg_mon_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(sovleg_mon_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(sovleg_mon_be_t10, sovereign_legitimacy__monarchical_reading, base_extractiveness, 10, 0.83).
narrative_ontology:measurement(sovleg_mon_be_t20, sovereign_legitimacy__monarchical_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(sovleg_mon_be_t30, sovereign_legitimacy__monarchical_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(sovleg_mon_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(sovleg_mon_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(sovleg_mon_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(sovleg_mon_su_t10, sovereign_legitimacy__monarchical_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(sovleg_mon_su_t20, sovereign_legitimacy__monarchical_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(sovleg_mon_su_t30, sovereign_legitimacy__monarchical_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(sovleg_mon_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(sovleg_mon_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
