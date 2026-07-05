% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Continuity Reading of Orthographic Legitimacy (Turkish Script Reform, 1928)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the orthographic
 *   legitimacy kernel: the claim that legitimate script choice derives from
 *   preserving unbroken access to a community's historical, religious, and
 *   literary corpus. Under this reading, the 1928 Turkish alphabet reform
 *   (Latin script replacing Ottoman Arabic script) reads primarily as a
 *   severance event rather than a modernization achievement — the
 *   constraint's low ε reflects that the mechanism at issue (script
 *   incompatibility) is close to a physical/informational fact: a population
 *   trained in one writing system cannot read another without dedicated
 *   retraining, full stop. What is contested is not that fact but which value
 *   should govern legitimacy given it. The instrumentalist reading
 *   (literacy/efficiency maximization) and the modernist reading (rupture
 *   with the Ottoman/Islamic past as a positive good) are separate
 *   constraints with their own ε and stakeholder structures — they are not
 *   measured here.
 *
 * KEY AGENTS:
 *   - arabic_script_literate_clergy: beneficiary under continuity framing — retains interpretive monopoly
 *   - post_reform_generations: primary victim — trapped, civilizational time horizon, cannot retroactively access severed corpus
 *   - turkish_republic_state: agenda_setter whose actual policy choice diverged from this reading's normative claim
 *   - linguistic_historians: analytical observers comparing reformed vs. non-reformed script transitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.28).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Continuity Reading of Orthographic Legitimacy (Turkish Script Reform, 1928)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, 'b5e5ddd8-9139-4fef-a4c1-7ccd76512d71').
narrative_ontology:cs_kernel_codification('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', distributed).
narrative_ontology:cs_authority_grounding('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', distributed).
narrative_ontology:cs_reading_relation('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', orthographic_legitimacy_kernel__modernist_reading, forecloses).
narrative_ontology:cs_reading_relation('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', foundational, textual_access_continuity_is_the_legitimacy_criterion).
narrative_ontology:cs_axiom_status(textual_access_continuity_is_the_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', textual_access_continuity_is_the_legitimacy_criterion, deontological).
narrative_ontology:cs_axiom('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', secondary, rupture_from_prior_tradition_is_not_itself_a_legitimating_value).
narrative_ontology:cs_axiom_status(rupture_from_prior_tradition_is_not_itself_a_legitimating_value, holdable).
narrative_ontology:cs_axiom_grounding('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', rupture_from_prior_tradition_is_not_itself_a_legitimating_value, conventional).
narrative_ontology:cs_reference_frame('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', ottoman_arabic_script_as_continuous_tradition).
narrative_ontology:cs_drift_state('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', post_1928_reform_contemporary, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('b5e5ddd8-9139-4fef-a4c1-7ccd76512d71', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, arabic_script_literate_clergy).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, pre_reform_literary_custodians).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, religious_text_readers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, ottoman_archive_researchers).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, orthographic_continuity_doctrine).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__continuity_reading, textual_tradition_preservation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain the ability to read Quranic Arabic, Ottoman religious texts, and centuries of jurisprudential commentary without translation. Their authority as interpreters of tradition depends on script continuity remaining the legitimate standard; a Latin-script standard would not remove their knowledge but would sever their monopoly on transmitting it to new generations.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_literate_clergy, beneficiary,
    moderate, generational, identity_locked, national).

% Scholars, archivists, and family libraries holding Ottoman-Arabic manuscripts. Under the continuity reading their custodianship is validated as preserving legitimate heritage; they can still read what they hold, but the audience able to receive it without specialist mediation shrinks with each cohort raised under the new alphabet.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, pre_reform_literary_custodians, beneficiary,
    moderate, generational, constrained, national).

% Educated exclusively in the Latin alphabet after 1928. Cannot read their grandparents' letters, mosque inscriptions, tombstones, or the bulk of the Ottoman state archive without formal training treated as a specialist skill rather than ordinary literacy. This is not a policy they can opt out of; it was fixed before they were born and the loss compounds with time as living interpreters die out.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, civilizational, trapped, national).

% Practicing Muslims seeking direct engagement with religious commentary written in Ottoman Arabic script now depend on transliterations, translations, or specialist clergy as intermediaries where earlier generations could read primary texts directly. Exit exists in principle (learn Arabic script as an elective skill) but is costly and marginal relative to ordinary schooling.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_text_readers, payer,
    powerless, generational, constrained, national).

% Historians and genealogists needing years of specialized paleographic training to read primary sources that were ordinary reading material for the pre-1928 population. They have professional exit (become a specialist) but the general population does not; their situation illustrates what continuity would have preserved as common competence.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, ottoman_archive_researchers, payer,
    moderate, biographical, mobile, national).

% Enacted and enforced the 1928 alphabet reform, mandating Latin script in all official, educational, and eventually most published contexts. From the continuity reading's vantage point, the state is not a beneficiary of preserved tradition but the actor whose choice determined whether continuity was preserved or severed — it made the opposite choice, which is why this reading registers the reform's effect as loss rather than the constraint it defends.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, turkish_republic_state, agenda_setter,
    institutional, civilizational, analytical, national).

% Study script reform as a case of state-imposed orthographic rupture. They can compare literacy outcomes, archive accessibility, and intergenerational transmission across reformed and non-reformed comparator states without being party to the loss themselves.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared, stable orthography lets any literate member of a linguistic community read texts produced by any other member across generations without specialist mediation — this is the genuine coordination problem orthographic continuity solves when it holds.
% TRANSFER_FUNCTION: Where continuity is broken, the arrangement transfers historical and religious textual access away from the general population and toward a shrinking specialist class (clergy, paleographers, archivists) who retain the older script — ordinary readers must pay in training cost or accept mediated access.
% ABSENT_VOICES: The generations born after 1928 who never chose the reform and cannot retroactively object were never in the room when the decision was made; their descendants' testimony about severed access to family and religious documents is the closest available proxy, and it is largely absent from the state's own legitimating narrative.
% DISAPPEARANCE_RATIONALE: If orthographic continuity with pre-1928 script had been preserved instead of severed, ordinary literate citizens today would read Ottoman archives, religious commentary, and family documents directly; the current arrangement (Latin script as sole legitimate standard) means that world does not exist, and the population's relationship to its own textual past has structurally rearranged around specialist mediation instead of common literacy.
% FOUNDING_PROBLEM: Access to a civilization's own historical, religious, and literary corpus should not require intermediaries — the founding claim of the continuity reading is that legitimate orthography is whatever preserves that access unbroken across generations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and archivists outside any pro-reform or pro-continuity political coalition (e.g. comparative script-reform scholarship, UNESCO literacy-heritage assessments) attest that post-1928 Turkish readers require specialist training to access pre-reform primary sources that earlier generations read as a matter of ordinary literacy — corroboration exists independent of clergy or reform-state beneficiaries, though it is thin and mostly academic rather than a live political constituency.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at T=96) because the continuity reading does not describe an extraction mechanism in the classic rent-seeking sense — no party is charging post-reform generations a toll. Instead it describes accumulating LOSS: a widening accessibility gap between what earlier generations could read as ordinary literacy and what later generations require specialist training to read. This is why extractiveness rises gently over the interval (0.05 to 0.28) rather than staying flat or spiking: the severance is real at T=0 but its cost compounds as living bilingual-script readers age out and intergenerational oral bridging attenuates. Accessibility collapse is authored very high (0.88) because once a generation is raised in Latin script only, the alternative (fluent Arabic-script literacy as common competence) has genuinely and almost completely foreclosed for the population at large — this is the mountain-like signature: the fact of incompatibility is not something political will can simply reverse for people already schooled. Resistance is authored moderate-low (0.35): there is real but minority resistance (religious communities maintaining Quranic-script literacy, heritage movements, academic advocacy) but no mass political movement to reverse the alphabet.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries under this reading are those who retained or could retain access to the pre-reform corpus — Arabic-script literate clergy and literary custodians — not because the reform benefited them but because the continuity value, if honored, would have preserved (not conferred) their common-literacy status rather than reducing it to specialist status. Victims are post-reform generations, religious text readers, and archive researchers, who bear the cost of severed access. The state is authored as agenda_setter rather than beneficiary because under the continuity reading the state's actual choice ran counter to the value being evaluated — it is the actor whose decision determined whether the constraint would hold, and it decided against continuity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — direct, unmediated intergenerational access to a civilization's textual heritage — is authored as dead in the sense that the specific institutional arrangement (Ottoman-Arabic script as the working alphabet) no longer holds and cannot be revived by policy alone once generations have been trained otherwise; the mountain-like accessibility_collapse metric captures this irreversibility. This prevents mislabeling the situation as ongoing pure extraction (there is no beneficiary actively profiting from the ongoing severance) while also refusing to launder it as costless modernization — the loss is real, borne unevenly, and does not disappear because the reform is now nearly a century old.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_instrumentalist_tradeoff,
    'Was the accessibility loss registered by the continuity reading a necessary cost of the literacy gains claimed by the instrumentalist reading, or were they structurally separable (e.g., could Arabic script have been phonetically reformed for the same literacy gains without full alphabet replacement)?',
    'Comparative study of contemporaneous scripts reforms that improved literacy without full alphabet replacement (e.g., simplified Arabic orthographies in other reforming states) to test whether the literacy gain required severance or merely simplification.',
    'If separable, the continuity reading''s victim set (post-reform generations) bore a cost that was not required to achieve the instrumentalist reading''s benefit, strengthening the case that the reform''s ε under this reading understates avoidable loss. If inseparable, the two readings describe a genuine tradeoff rather than one reading identifying pure avoidable harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_instrumentalist_tradeoff, empirical, 'Whether literacy gains required full script severance or could have preserved continuity.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three kernel readings (continuity, instrumentalist, modernist) genuinely commensurable evaluations of the same historical act, or do they pick out different acts entirely (the reform-as-cultural-severance vs. the reform-as-literacy-policy vs. the reform-as-civilizational-alignment)?',
    'Examine primary sources from the 1928 reform debate itself (parliamentary records, Atatürk''s stated rationale, contemporary opposition arguments) to determine whether framers themselves treated these as one integrated justification or as separable claims that happened to converge on one policy.',
    'If the framers treated the three justifications as one integrated package, later decomposition into separate kernel readings is an analytical convenience that may understate how tightly coupled the historical decision actually was; if the framers themselves argued past each other using different justifications, the decomposition tracks a real historical fracture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel readings are cleanly separable claims or artifacts of retrospective analytical decomposition.').

omega_variable(
    false_summit_natural_law_framing,
    'Is script-incompatibility genuinely a mountain-like physical/informational fact independent of political choice, or does declaring it ''natural'' obscure that the SEVERITY of severance (total replacement vs. gradual bilingual transition vs. supplementary teaching) was itself a constructed policy choice that benefited specific actors (state-building nationalists seeking rupture)?',
    'Compare severity of severance across states that underwent script reform via different transition policies (abrupt vs. phased vs. dual-script periods) to test whether the accessibility collapse measured here is intrinsic to any script change or specific to the abrupt implementation chosen in this case.',
    'If severity was a policy choice rather than an intrinsic feature of script incompatibility, then declaring beneficiaries (arabic_script_literate_clergy) alongside emerges_naturally=true is exactly the false-summit signature: a constructed severity dressed as natural fact. This omega documents that ambiguity as required by the FSM schema gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_framing, conceptual, 'Whether the mountain-like accessibility collapse is intrinsic to script change or an artifact of the specific abrupt-transition policy chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 12, 0.04).
narrative_ontology:measurement_basis(orth_tr_t12, observed).
narrative_ontology:measurement(orth_tr_t24, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 24, 0.06).
narrative_ontology:measurement_basis(orth_tr_t24, observed).
narrative_ontology:measurement(orth_tr_t48, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 48, 0.09).
narrative_ontology:measurement_basis(orth_tr_t48, observed).
narrative_ontology:measurement(orth_tr_t72, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 72, 0.12).
narrative_ontology:measurement_basis(orth_tr_t72, observed).
narrative_ontology:measurement(orth_tr_t96, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 96, 0.15).
narrative_ontology:measurement_basis(orth_tr_t96, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 12, 0.09).
narrative_ontology:measurement_basis(orth_be_t12, observed).
narrative_ontology:measurement(orth_be_t24, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 24, 0.13).
narrative_ontology:measurement_basis(orth_be_t24, observed).
narrative_ontology:measurement(orth_be_t48, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 48, 0.19).
narrative_ontology:measurement_basis(orth_be_t48, observed).
narrative_ontology:measurement(orth_be_t72, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 72, 0.24).
narrative_ontology:measurement_basis(orth_be_t72, observed).
narrative_ontology:measurement(orth_be_t96, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 96, 0.28).
narrative_ontology:measurement_basis(orth_be_t96, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(orthographic_legitimacy_kernel__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the orthographic_legitimacy_kernel applied to the 1928 Turkish alphabet reform. continuity_reading (this file) treats the reform as severance from historical/religious/literary tradition, scoring low-moderate ε with post-reform generations as victims. instrumentalist_reading treats the same reform as literacy/administrative-efficiency policy, expected to score as a Rope or beneficial coordination mechanism with different or no victims. modernist_reading treats the reform as valorized civilizational rupture, potentially inverting the beneficiary/victim assignment entirely (the state and Westernizing elites as beneficiaries, traditionalists as the class whose objection is delegitimized rather than harmed). All three are linked here to enable contamination/coupling analysis across the kernel family; none of the three should be read as a different observation of a shared ε — each has its own stable ε per the invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
