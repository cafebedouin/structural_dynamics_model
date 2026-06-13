% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   Lycurgan law as instantiated in this reading is a constitutional
 *   commitment whose core structure is claimed to be immutable—enacted by the
 *   legendary lawgiver Lycurgus, allegedly unchangeable without destroying
 *   Sparta's identity. This reading interprets that immutability claim as
 *   structurally snare-like: the constraint persists not because it continues
 *   to solve the founding coordination problem (manpower unity post-Persian
 *   Wars) but because the ephorate and oligarchy have institutional interest
 *   in its perpetuation. The constraint prevents demographic adaptation
 *   measures that would have preserved the state. The measurement series
 *   documents rising theater_ratio and suppression_requirement relative to
 *   founding extractiveness: the constraint operates with decreasing
 *   coordination function and increasing performative maintenance as the
 *   demographic crisis deepens.
 *
 * KEY AGENTS:
 *   - conservative_ephorate: institutional agenda-setter; interprets law as immutable; blocks reform proposals
 *   - agrarian_oligarchy: beneficiary; maintains power through land monopoly and property restrictions
 *   - spartiate_citizen_body: powerless payers; trapped by identity-lock (Spartiate status inseparable from legal obligation); cannot exit or adapt
 *   - younger_sons_and_hypomeiones: dispossessed; fall below mess threshold and lose citizenship; forbidden from alternative livelihoods
 *   - women_and_perioikoi: excluded; bear costs without voice; cannot participate in reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '9bc5e67c-a11c-4f4c-baca-ae13077f11dc').
narrative_ontology:cs_kernel_codification('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', fixed_text).
narrative_ontology:cs_authority_grounding('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', extraction).
narrative_ontology:cs_interpretation_layer_present('9bc5e67c-a11c-4f4c-baca-ae13077f11dc').
narrative_ontology:cs_reading_relation('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', foundational, constitutional_immutability_enforced).
narrative_ontology:cs_axiom_status(constitutional_immutability_enforced, holdable).
narrative_ontology:cs_axiom_grounding('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', constitutional_immutability_enforced, empirically_contingent).
narrative_ontology:cs_axiom('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', foundational, immutability_causes_demographic_trap).
narrative_ontology:cs_axiom_status(immutability_causes_demographic_trap, holdable).
narrative_ontology:cs_axiom_grounding('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', immutability_causes_demographic_trap, empirically_contingent).
narrative_ontology:cs_reference_frame('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', immutable_ancestral_law).
narrative_ontology:cs_drift_state('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', hellenistic_collapse_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('9bc5e67c-a11c-4f4c-baca-ae13077f11dc', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, conservative_ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, agrarian_oligarchy).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizen_body).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, women_and_perioikoi).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, rising_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, younger_sons_and_dispossessed).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, constitutional_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, unchangeable_ancestral_law_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five annually elected magistrates who interpret and enforce Lycurgan law. They wield veto power over any proposal to reform citizenship, property, military training, or marriage rules. Their authority rests on the claim that they are guardians of immutable ancestral law; any revision would undermine their legitimacy as interpreters rather than legislators. They block demographic adaptation measures—relaxing citizenship restrictions, allowing wealth mobility, permitting alternative livelihoods—that would directly threaten their gatekeeping power.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, conservative_ephorate, agenda_setter,
    institutional, generational, arbitrage, regional).

% Landholding families who retain disproportionate control over allotted plots (kleros system). The law's prohibition on selling land, dividing inheritance, or entering non-military trades locks younger sons, daughters, and dispossessed Spartiates into economic roles that cannot support families. Oligarchs benefit from labor control and the political power of scarcity—fewer citizens means fewer claims on state resources and higher proportional influence per remaining family.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, agrarian_oligarchy, beneficiary,
    powerful, biographical, constrained, regional).

% Male citizens obligated to full-time military training (agoge), communal messes, and participation in the warfare machine. Women citizens are confined to reproductive roles and martial ideology. Marriage, property holding, and career are dictated by law. Citizens cannot emigrate without losing status and citizenship. Those who fall below the property threshold for mess participation lose citizenship entirely (hypomeiones). The declining male population makes military service increasingly unsustainable, but the law forbids adaptation: admitting non-citizens, relaxing training regimens, allowing economic diversification, or permitting exit—all routes to survival are legislatively closed.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizen_body, payer,
    powerless, biographical, identity_locked, regional).

% Women are trapped in reproductive and domestic roles; they have no political voice. Perioikoi (non-citizen inhabitants) are excluded from military and political participation entirely. The law creates a rigid caste structure: perioikoi cannot become citizens, women cannot hold property or vote. Both groups bear the costs of the Spartiate system (labor, taxation, subservience) without any possibility of reform through political participation or migration.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, women_and_perioikoi, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, women_and_perioikoi, excluded).

% Citizens whose families lose landholdings or who have no inheritance fall below the mess-participation threshold and are stripped of full citizenship (hypomeiones). The law forbids them from earning alternative income through trade, craft, or merchant work—the only paths out are military service (increasingly untenable as population collapses) or exile. Their identity as Spartiates is legally extinguished, yet the law forbids them from becoming something else within the system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, younger_sons_and_dispossessed, payer,
    powerless, biographical, identity_locked, regional).

% The council of elders and magistrates maintain the fiction that Lycurgan law is immutable divine ordinance, even as demographic reality shows the system collapsing. They are the analytical seat observing the contradiction between claimed constitutional law and lived structural failure.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_and_gerousia, observer,
    institutional, generational, analytical, regional).

% Later Greek and modern scholars who document the Spartiate population decline from ~8,000 adult males in 480 BCE to fewer than 1,000 by the 4th century BCE. They observe that the constraint's claimed immutability prevented adaptation even as demographic collapse made the system unsustainable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, external_philosophers_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, agrarian_oligarchy).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally coordinated Sparta's unique warrior society: rigid training regimen, common messes, land allotment, prohibition on wealth accumulation, and reproductive discipline created a unified military caste dedicated to territorial defense against numerical superiority. The constraint solved a genuine collective-action problem in classical Greece.
% TRANSFER_FUNCTION: The constraint transfers economic surplus, labor, and reproductive potential from the wider population (women, perioikoi, younger sons, the dispossessed) to the agrarian oligarchy and the military apparatus. It moves decision-making power from the citizen body to the conservative ephorate, who claim to administer unchangeable law rather than make political choices.
% ABSENT_VOICES: Younger Spartiates who have fallen out of the citizen body; women who bear reproductive burden without political voice; perioikoi whose labor supports the system but who cannot participate in reform; exiled Spartiates who chose emigration over identity-lock. None of these groups appear at the decision table when the ephorate interprets law.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (i.e., if Lycurgan law became revisable), Sparta could have adapted: admitting perioikoi and hypomeiones to citizenship, permitting economic diversification, allowing inheritance division, opening migration. These measures would have preserved the state as a military power and prevented the demographic collapse that reduced Sparta to irrelevance by the Hellenistic era. The constraint's disappearance would have meant institutional survival rather than extinction.
% FOUNDING_PROBLEM: After the Persian Wars, Sparta faced a manpower crisis in the contest with Athens. The response was to codify a unique warrior society via Lycurgan law—establishing a training system, common living, land allotment, and reproductive discipline to maximize military cohesion and martial output. The founding problem was: how to maintain unified warrior identity and prevent wealth stratification from fracturing military unity?
% FOUNDING_PROBLEM_CORROBORATION: Historians (Plutarch, Xenophon, modern scholarship) document that by the 4th century BCE, the manpower crisis that Lycurgan law was designed to solve had been superseded by a different crisis: demographic collapse. The constraint persisted in increasingly theatrical form (endless reaffirmations of immutability, fictionalized adherence) while the underlying population fell below viable reproduction thresholds. The founding problem was solved by the 5th century; the constraint's persistence became pure institutional inertia and oligarchic control.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as snare because: (1) extractiveness is high (0.82 at interval end) and rising monotonically—the constraint extracts compulsory labor, economic surplus, reproductive potential, and political voice; (2) suppression is higher still (0.88)—the constraint's persistence depends on active enforcement by the ephorate, blocking any proposal to admit new citizens, relax property rules, allow emigration, or diversify livelihoods; (3) theater ratio rises sharply from 0.22 to 0.67—in the later interval the constraint's stated function (unified warrior preparation) is increasingly theater, while actual enforcement focuses on maintaining oligarchic control and preventing demographic adaptation; (4) accessibility_collapse is high (0.79) but not complete—exit theoretically exists (emigration, voluntary exile) but carries identity destruction and loss of all status, making it identity-locked rather than genuinely available; (5) resistance is substantial (0.71)—the constraint meets real resistance from dispossessed citizens, hypomeiones, and women, evidenced by demographic collapse driven partly by voluntary emigration and family limitation. The founding coordination problem (manpower unity) is solved by 380 BCE; persistence from 380–330 is pure institutional inertia and oligarchic gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the ephorate's and oligarchy's seats, the constraint appears as sacred constitutional law requiring absolute fidelity—they do not experience it as extractive because they benefit from its operation and their role as guardians of immutable law sources their authority. From the spartiate citizen's seat (especially younger sons and hypomeiones), the constraint is pure suppression: it locks them into an arrangement that no longer serves the collective purpose but only serves oligarchic power. From the analytical seat (historians observing across centuries), the constraint's function shifted around 400 BCE from coordination (solving manpower fragmentation) to pure extraction (maintaining oligarchic control and landmonopoly). The engine should compute dramatically different type verdicts per seat: the ephorate seat may compute something closer to rope (they experience coordination and legitimacy); the oligarchic seat should compute snare (naked extraction); the citizen and dispossessed seats should compute snare-with-high-suppression (they experience pure constraint without benefit). The asymmetry is the measurement this story exists to enable.
 *
 * DIRECTIONALITY LOGIC:
 *   The ephorate derives directionality close to 0.0 (full beneficiary): they gain legitimacy, administrative power, and gatekeeping authority from their role as interpreters of immutable law. The oligarchy derives directionality near 0.3–0.4 (net beneficiary, some resistance to extraction's legitimacy): they gain from land monopoly and labor control, though their private wealth is technically constrained by the anti-accumulation ethos. The spartiate citizen derives directionality near 0.85–0.95 (full target): they bear compulsory labor, reproductive obligation, economic constraint, and face identity-lock that makes exit near-impossible. Younger sons and hypomeiones derive directionality at 1.0 (complete extraction): they are stripped of citizenship, forbidden alternative livelihoods, and have no benefit stream. Women and perioikoi derive directionality at 0.9+ (complete targets): they bear burden without any stake in the constraint's decision structure. The override entries are not needed here—the derivation chain from beneficiary/victim + exit options produces correct directionality automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: founding_problem_status = dead (the manpower crisis is solved by the hellenistic period) while disappearance_verdict = world_rearranges (the constraint's removal would have allowed adaptation and survival). The theater_ratio rising to 0.67 shows increasing proportion of enforcement activity devoted to performative maintenance (endless reaffirmations of immutability) rather than functional enforcement. The constraint persists not because the population believes in it (resistance = 0.71 shows substantial rejection) but because the ephorate and oligarchy have institutional interest in its perpetuation and the suppression apparatus (0.88) is sufficient to hold it despite mounting demographic failure. This is the classic piton-versus-snare boundary: if the beneficiaries were the ephorate and oligarchy PASSIVELY enjoying the surplus without active enforcement, it would be piton (dead function, inertial persistence). But suppression_requirement = 0.88 shows active, intensive enforcement. The constraint requires constant veto, constant reaffirmation of immutability doctrine, constant blocking of reform proposals. That active effort to suppress adaptation options (the contrapositive of passive inertia) classifies it as snare: the constraint extracts from the full citizen body and the dispossessed, and that extraction must be actively defended against reform pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_constraint_vs_actual_adaptation,
    'To what extent was Lycurgan law actually unchanging, versus what extent did covert or disguised adaptation occur within the immutability framework?',
    'Detailed textual and institutional analysis of legislative changes (property reforms, citizenship admissions, women''s roles, perioikoi integration) across the 480–330 interval; comparison of formal law-text claims against actual practices documented in inscriptions and historical accounts.',
    'If substantial covert adaptation occurred, the immutability constraint is better classified as a fiction (rope or adaptive_fiction_reading) masking real reform. If immutability was genuinely enforced despite population collapse, the snare classification is confirmed and the constraint is a pure structural trap. The true ε value depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_constraint_vs_actual_adaptation, empirical, 'Whether the immutability constraint was enforced or circumvented through hidden adaptation.').

omega_variable(
    demographic_collapse_causation,
    'To what extent did the immutability constraint (prohibition on citizenship admission, property rules, livelihoods, emigration) directly cause the Spartiate population collapse, versus what extent was collapse driven by external factors (warfare losses, disease, economic competition with Athens)?',
    'Comparative demographic analysis: model Sparta''s population under counterfactual scenarios with relaxed constraints (perioikoi citizenship admission, hypomeiones rehabilitation, emigration allowed, merchant/craft work permitted); compare against observed population trajectory and against similar city-states with more flexible citizenship rules.',
    'If the constraint accounts for majority of demographic decline, the snare classification is strongly confirmed. If external factors dominated, the constraint is a secondary factor that exacerbated rather than caused collapse—closer to piton (inertial) than snare (actively extractive-unto-collapse). The causation chain determines the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_collapse_causation, empirical, 'Causal weight of the immutability constraint on demographic collapse.').

omega_variable(
    identity_lock_mechanism_scope,
    'For different cohorts (younger sons, hypomeiones, women, perioikoi), what proportion of the measured suppression is structural (legal barriers: cannot trade, cannot emigrate, cannot hold property) versus internalized (Spartiate identity fused with obligation such that exit would psychologically constitute self-annihilation)?',
    'Historical evidence of voluntary emigration, family limitation, and dissent within the Spartiate body; accounts of those who chose exile and whether they report identity destruction or relief; gender and age-stratified evidence of resistance and acceptance.',
    'If suppression is heavily internalized (identity-locked), the constraint''s effective suppression is higher than the structural measure suggests—victims carry the suppression with them after (hypothetical) exit, making the constraint more snare-like. If suppression is primarily structural (legal barriers), exit post-constraint-removal would be functional and suppress would decline sharply, suggesting the constraint is more purely extractive but potentially reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scope, empirical, 'Structural versus internalized suppression in the Spartiate identity-lock.').

omega_variable(
    kernel_reading_boundary_assumption,
    'Is the distinction between immutability-as-sacred-law and immutability-as-demographic-trap a genuine structural distinction, or is it a difference in evaluative framing of a single constraint?',
    'Formal analysis of ε-invariance for this constraint under the three readings: does the constraint''s structural extraction, suppression, and beneficiary set remain stable across readings, or does the reading choice materially change what is being measured? If ε is invariant, the three readings are three perspectives on one constraint; if ε differs meaningfully, they are three constraints (kernel decomposition is valid).',
    'If ε is invariant across readings, the kernel is a single constraint with multiple interpretive lenses—the schema should unify them. If ε differs, the schema''s three-constraint decomposition is validated. The correctness of the committer frame (kernel + readings) depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_assumption, conceptual, 'Whether the three sibling readings represent different constraints (valid kernel decomposition) or different perspectives on one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 480, 330).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t480, lycurgan_laws__demographic_trap_reading, theater_ratio, 480, 0.22).
narrative_ontology:measurement_basis(lycu_tr_t480, observed).
narrative_ontology:measurement(lycu_tr_t420, lycurgan_laws__demographic_trap_reading, theater_ratio, 420, 0.31).
narrative_ontology:measurement_basis(lycu_tr_t420, observed).
narrative_ontology:measurement(lycu_tr_t380, lycurgan_laws__demographic_trap_reading, theater_ratio, 380, 0.45).
narrative_ontology:measurement_basis(lycu_tr_t380, observed).
narrative_ontology:measurement(lycu_tr_t340, lycurgan_laws__demographic_trap_reading, theater_ratio, 340, 0.58).
narrative_ontology:measurement_basis(lycu_tr_t340, observed).
narrative_ontology:measurement(lycu_tr_t330, lycurgan_laws__demographic_trap_reading, theater_ratio, 330, 0.67).
narrative_ontology:measurement_basis(lycu_tr_t330, observed).

% Extraction over time
narrative_ontology:measurement(lycu_be_t480, lycurgan_laws__demographic_trap_reading, base_extractiveness, 480, 0.45).
narrative_ontology:measurement_basis(lycu_be_t480, observed).
narrative_ontology:measurement(lycu_be_t420, lycurgan_laws__demographic_trap_reading, base_extractiveness, 420, 0.52).
narrative_ontology:measurement_basis(lycu_be_t420, observed).
narrative_ontology:measurement(lycu_be_t380, lycurgan_laws__demographic_trap_reading, base_extractiveness, 380, 0.63).
narrative_ontology:measurement_basis(lycu_be_t380, observed).
narrative_ontology:measurement(lycu_be_t340, lycurgan_laws__demographic_trap_reading, base_extractiveness, 340, 0.75).
narrative_ontology:measurement_basis(lycu_be_t340, observed).
narrative_ontology:measurement(lycu_be_t330, lycurgan_laws__demographic_trap_reading, base_extractiveness, 330, 0.82).
narrative_ontology:measurement_basis(lycu_be_t330, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t480, lycurgan_laws__demographic_trap_reading, suppression_requirement, 480, 0.64).
narrative_ontology:measurement_basis(lycu_su_t480, observed).
narrative_ontology:measurement(lycu_su_t420, lycurgan_laws__demographic_trap_reading, suppression_requirement, 420, 0.71).
narrative_ontology:measurement_basis(lycu_su_t420, observed).
narrative_ontology:measurement(lycu_su_t380, lycurgan_laws__demographic_trap_reading, suppression_requirement, 380, 0.78).
narrative_ontology:measurement_basis(lycu_su_t380, observed).
narrative_ontology:measurement(lycu_su_t340, lycurgan_laws__demographic_trap_reading, suppression_requirement, 340, 0.84).
narrative_ontology:measurement_basis(lycu_su_t340, observed).
narrative_ontology:measurement(lycu_su_t330, lycurgan_laws__demographic_trap_reading, suppression_requirement, 330, 0.88).
narrative_ontology:measurement_basis(lycu_su_t330, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.14).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The constraint 'Lycurgan laws' decomposes into three ε-distinct readings of the immutability kernel. This constraint (demographic_trap_reading) treats immutability as a structural snare whose unrevisability caused collapse. The sibling readings (sacral_fidelity_reading, adaptive_fiction_reading) instantiate the same kernel with different ε values, beneficiary structures, and classifications. All three stories link via affects_constraints; commentary.kernel_context documents the decomposition in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
