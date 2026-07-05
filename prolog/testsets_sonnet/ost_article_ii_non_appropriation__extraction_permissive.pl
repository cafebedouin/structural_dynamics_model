% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II — Extraction-Permissive Reading (Resource Enclosure via Capability Gate)
 *   domain: international_law/space_policy/commons_governance
 *
 * SUMMARY:
 *   This story instantiates the extraction-permissive reading of Article II
 *   of the 1967 Outer Space Treaty: the claim that the treaty's bar on
 *   sovereign territorial claims and 'national appropriation by claim of
 *   sovereignty, by means of use or occupation, or by any other means' does
 *   not reach private ownership of resources once extracted, because
 *   extraction converts an in-situ commons resource into a moveable chattel
 *   severable from the territorial question. This reading has been
 *   operationalized since roughly the 2010s through domestic legislation (the
 *   US 2015 Commercial Space Launch Competitiveness Act, Luxembourg's 2017
 *   space resources law, UAE and Japan analogues) that grants firms ownership
 *   of what they extract without national territorial claims. The structural
 *   delta from the sibling readings is a high-extractiveness ledger: access
 *   is gated by launch/extraction technology and by which flag state is
 *   willing to grant recognition, there is no compensation mechanism running
 *   back to non-spacefaring treaty parties, and the enclosure proceeds by
 *   fait accompli (licensing statutes and eventual extraction activity)
 *   rather than by any formal annexation event a court or UN body could
 *   easily adjudicate.
 *
 * KEY AGENTS:
 *   - spacefaring_launch_states: agenda_setter — legislate the extraction-permissive interpretation domestically
 *   - private_asteroid_mining_firms: beneficiary — capture extracted resource value under flag-state license
 *   - flag_state_licensing_regimes: agenda_setter/beneficiary — administer and profit from first-mover legal recognition
 *   - non_spacefaring_states: payer — bear the enclosure cost with no capability to participate or be compensated
 *   - equatorial_launch_states_without_capability: payer — geographically proximate but capability-excluded
 *   - future_generations_of_claimants: excluded — no standing to object to current extraction
 *   - international_space_law_scholars: observer — document the treaty-text/domestic-statute divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.72).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.58).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.72).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II — Extraction-Permissive Reading (Resource Enclosure via Capability Gate)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_policy/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '7f5effae-c990-42fc-b306-b428b92a0674').
narrative_ontology:cs_kernel_codification('7f5effae-c990-42fc-b306-b428b92a0674', fixed_text).
narrative_ontology:cs_authority_grounding('7f5effae-c990-42fc-b306-b428b92a0674', distributed).
narrative_ontology:cs_reading_relation('7f5effae-c990-42fc-b306-b428b92a0674', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('7f5effae-c990-42fc-b306-b428b92a0674', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('7f5effae-c990-42fc-b306-b428b92a0674', foundational, extraction_severs_resource_from_territorial_claim).
narrative_ontology:cs_axiom_status(extraction_severs_resource_from_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('7f5effae-c990-42fc-b306-b428b92a0674', extraction_severs_resource_from_territorial_claim, conventional).
narrative_ontology:cs_axiom('7f5effae-c990-42fc-b306-b428b92a0674', secondary, domestic_legislative_recognition_suffices_for_title).
narrative_ontology:cs_axiom_status(domestic_legislative_recognition_suffices_for_title, holdable).
narrative_ontology:cs_axiom_grounding('7f5effae-c990-42fc-b306-b428b92a0674', domestic_legislative_recognition_suffices_for_title, conventional).
narrative_ontology:cs_reference_frame('7f5effae-c990-42fc-b306-b428b92a0674', treaty_text_plain_meaning_1967).
narrative_ontology:cs_drift_state('7f5effae-c990-42fc-b306-b428b92a0674', post_commercial_launch_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f5effae-c990-42fc-b306-b428b92a0674', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_asteroid_mining_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, flag_state_licensing_regimes).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, equatorial_launch_states_without_capability).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, res_nullius_extraction_doctrine).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, sovereign_claim_private_ownership_severability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pass domestic legislation (e.g. commercial space resource acts) recognizing private ownership of extracted materials while formally disclaiming territorial sovereignty, satisfying Article II's letter while enabling their licensed firms to capture resource value. They control the launch capability that gates who can act on this reading at all.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_launch_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Extract and claim ownership of off-world materials under flag-state licenses, treating physical possession plus domestic legal recognition as sufficient title. They benefit directly from the ambiguity: the treaty text does not bar them, and no international body currently has standing to stop them.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_asteroid_mining_firms, beneficiary,
    powerful, biographical, mobile, global).

% Administer the domestic legal frameworks that convert extraction into recognized private title. They set the terms of access for firms under their jurisdiction and collect regulatory authority and diplomatic leverage from being first movers on the interpretive question.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, flag_state_licensing_regimes, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, flag_state_licensing_regimes, beneficiary).

% Are signatories to the same treaty but possess no launch capability to act on the extraction-permissive reading themselves. They bear the cost of a resource commons being enclosed by capability rather than by any process they can participate in, with no compensation mechanism and no realistic path to contest individual extraction claims after the fact.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    powerless, generational, trapped, global).

% Occupy geographically advantageous launch positions but lack the industrial base to exploit them, and so cannot convert geography into extraction rights under this reading. They are structurally similar in exposure to the broader non-spacefaring bloc but distinguished by the irony of proximity without capacity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, equatorial_launch_states_without_capability, payer,
    powerless, generational, trapped, national).

% Have no seat at all in current extraction decisions; resources claimed now under fait accompli enclosure are unavailable to whatever regime or population might exist decades hence. They cannot object because they do not yet exist as a constituency with standing.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations_of_claimants, excluded,
    powerless, civilizational, trapped, universal).

% Analyze the gap between the treaty's non-appropriation language and domestic resource-ownership statutes, documenting the divergence without power to resolve it. Their scholarship is cited by all sides but binds none.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_space_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits productive use of extracted space resources without requiring a settled multilateral allocation regime, allowing investment and technological development to proceed under legal uncertainty rather than waiting for consensus that may never form.
% TRANSFER_FUNCTION: Moves de facto control over extractable off-world resources from the undifferentiated body of treaty signatories to whichever states possess launch capability and choose to license extraction, with no transfer payment or compensation flowing back to excluded parties.
% ABSENT_VOICES: Non-spacefaring states and future populations who would object that this reading permits enclosure indistinguishable in effect from territorial appropriation are structurally absent from the venues (domestic legislatures of spacefaring states) where this reading is being operationalized; they were present at the treaty's original 1967 negotiation but have no comparable venue for the subsequent domestic-law reinterpretation.
% DISAPPEARANCE_RATIONALE: Spacefaring states and their licensed firms would say the world rearranges catastrophically — investment collapses without secure title. Non-spacefaring states and conservation-reading proponents would say the world is largely unchanged for humanity as a whole, since no extraction economy currently operates at meaningful scale; the dispute is over a mostly anticipatory legal structure rather than an operating one.
% FOUNDING_PROBLEM: The 1967 treaty needed to prevent a repeat of terrestrial colonial land-grabs in space while still permitting scientific and eventually commercial activity; the drafters left the extraction-ownership question deliberately unresolved because no extraction technology existed to force the issue.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring-state legislators and mining-firm counsel attest the founding problem (preventing sovereign land-grabs) is satisfied because no state claims territory; independent international-law scholars and G77-aligned diplomats attest from outside the beneficiary set that the founding problem was broader — preventing unilateral enclosure of common resources — and that the extraction-permissive reading reproduces the substance of the problem the treaty was built to prevent under a different legal label.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, contested).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the reading's entire structural function is to convert a commons resource into privately held title without any allocation mechanism reaching the excluded majority of treaty parties — this is a real transfer, not merely a permissive silence. Suppression is moderate-high (0.58) and rising over the measured interval: it is not backed by force in the classical sense, but by the structural fact that only capability-holders can act, and by increasingly entrenched domestic legal infrastructure that forecloses contestation after the fact (extraction, once physically completed, is nearly impossible to reverse through international process — this is the fait accompli mechanism named in the structural delta). Theater ratio is moderate (0.40) and rising: an increasing share of the diplomatic and legal activity around this reading (Artemis Accords signature ceremonies, COPUOS working-group sessions) performs multilateral legitimacy while the substantive extraction-recognition work happens entirely in domestic legislatures acting unilaterally. Accessibility collapse (0.62) reflects that once a flag state's licensing regime and a firm's extraction infrastructure exist, alternative allocation regimes become progressively harder to negotiate into existence — the fait accompli logic. Resistance (0.55) reflects real, organized objection from the G77 bloc and conservation-reading scholars, but objection that has not yet translated into binding counter-measures.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (spacefaring states, licensing regimes), this reading looks like a rope: it solves the genuine coordination problem of enabling investment under legal uncertainty, and no one is being coerced since Article II's territorial bar remains fully intact. From the payer seat (non-spacefaring states), the same structure looks like a tangled rope shading toward snare: coordination for spacefaring states purchased through uncompensated exclusion of everyone else, enforced not by treaty mechanism but by the practical irreversibility of completed extraction. The engine should compute this divergence from the structural data (beneficiary/victim declarations, differentiated exit options) rather than from either side's own characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring launch states and their licensed firms sit near the full-beneficiary end: they set the interpretive terms, capture the extracted value, and retain arbitrage-grade exit (they can forum-shop among flag states for the most permissive licensing regime). Non-spacefaring states and capability-poor equatorial states sit near the full-target end: trapped exit (they cannot exit the treaty framework without losing whatever normative leverage they retain, and cannot act on the permissive reading without capability they lack), bearing a real structural cost with no compensation. Future claimants are the extreme case — civilizational time horizon, universal scope, zero present standing, and no mechanism by which their interest could be represented even in principle under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing colonial-style unilateral enclosure of a common resource — is contested as live or dead depending on seat. If one reads 'enclosure' narrowly as formal territorial sovereignty claims, the founding problem is dead: no state has planted a flag and claimed sovereignty. If one reads 'enclosure' functionally as unilateral capture of common resource value without multilateral process, the founding problem is very much alive and this reading is actively reproducing it under a different legal vocabulary. This is why founding_problem_status is authored as contested rather than resolved in either direction — the mismatch between a 'dead' status (per beneficiaries) and a 'world_rearranges'-leaning disappearance profile (per non-beneficiary corroborators) is exactly the zombie-mandate signature R5 is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_severability_from_appropriation,
    'Is the ordinary meaning of Article II''s prohibition on national appropriation ''by any other means'' broad enough to capture unilateral resource extraction, or does extraction genuinely sever from the territorial-claim concept the treaty targets?',
    'An authoritative international judicial or arbitral ruling (e.g. ICJ advisory opinion, or a binding decision under a future Article XI-analogue regime) interpreting ''use or occupation... or by any other means'' against a state''s domestic extraction-ownership statute.',
    'If extraction is found to fall within ''any other means,'' this reading is foreclosed and the commons_conservation reading becomes the sole legally sound reading, converting the constraint''s classification from tangled_rope toward snare (no coordination cover survives). If extraction is found genuinely severable, this reading''s current operation is legally vindicated and the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_severability_from_appropriation, conceptual, 'Whether extraction is legally severable from the appropriation the treaty bars — the core interpretive fork of the kernel.').

omega_variable(
    fait_accompli_irreversibility,
    'Once a firm has physically extracted and returned resources to Earth (or established in-situ processing) under a flag-state license, is any subsequent multilateral regime realistically capable of unwinding or taxing that extraction retroactively?',
    'Track whether any future international regime (Article XI-analogue) attempts retroactive application to already-completed extraction, and whether such attempts succeed.',
    'If retroactive application proves impossible in practice, the fait accompli mechanism is confirmed as the reading''s true enforcement mechanism (rather than legal argument), which strengthens the case for classifying this reading''s operation as extractive enclosure regardless of its formal legal defensibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_irreversibility, empirical, 'Whether completed extraction is practically irreversible even if later found legally unauthorized.').

omega_variable(
    compensation_mechanism_absence,
    'Is the complete absence of any compensation or benefit-sharing mechanism for non-spacefaring states an incidental gap in current domestic statutes, or a structural feature this reading requires to function as currently practiced?',
    'Examine whether any spacefaring state''s domestic resource-ownership legislation includes voluntary benefit-sharing provisions, and whether industry has resisted such provisions when proposed in COPUOS or Artemis Accords negotiations.',
    'If benefit-sharing has been actively resisted by beneficiary states/firms when proposed, this supports classifying the absence as structural extraction rather than incidental gap, reinforcing the tangled_rope-toward-snare reading; if benefit-sharing is merely unaddressed and uncontested, the gap may be closable without disturbing the underlying reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compensation_mechanism_absence, empirical, 'Whether the lack of compensation to excluded states is structural to this reading or merely an unaddressed policy gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.05).
narrative_ontology:measurement_basis(ost__tr_t1967, observed).
narrative_ontology:measurement(ost__tr_t1990, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1990, 0.1).
narrative_ontology:measurement_basis(ost__tr_t1990, observed).
narrative_ontology:measurement(ost__tr_t2015, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2015, 0.2).
narrative_ontology:measurement_basis(ost__tr_t2015, observed).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.3).
narrative_ontology:measurement_basis(ost__tr_t2020, observed).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.36).
narrative_ontology:measurement_basis(ost__tr_t2025, observed).
narrative_ontology:measurement(ost__tr_t2030, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2030, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2030, projected).
narrative_ontology:measurement(ost__tr_t2035, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2035, 0.4).
narrative_ontology:measurement_basis(ost__tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement_basis(ost__be_t1967, observed).
narrative_ontology:measurement(ost__be_t1990, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(ost__be_t1990, observed).
narrative_ontology:measurement(ost__be_t2015, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(ost__be_t2015, observed).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement_basis(ost__be_t2020, observed).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(ost__be_t2025, observed).
narrative_ontology:measurement(ost__be_t2030, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2030, 0.7).
narrative_ontology:measurement_basis(ost__be_t2030, projected).
narrative_ontology:measurement(ost__be_t2035, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2035, 0.72).
narrative_ontology:measurement_basis(ost__be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.15).
narrative_ontology:measurement_basis(ost__su_t1967, observed).
narrative_ontology:measurement(ost__su_t1990, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement_basis(ost__su_t1990, observed).
narrative_ontology:measurement(ost__su_t2015, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement_basis(ost__su_t2015, observed).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement_basis(ost__su_t2020, observed).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.52).
narrative_ontology:measurement_basis(ost__su_t2025, observed).
narrative_ontology:measurement(ost__su_t2030, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2030, 0.57).
narrative_ontology:measurement_basis(ost__su_t2030, projected).
narrative_ontology:measurement(ost__su_t2035, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2035, 0.58).
narrative_ontology:measurement_basis(ost__su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, artemis_accords_resource_utilization_provisions).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'Article II non-appropriation principle,' per the ε-invariance principle: the three candidate interpretations (extraction-permissive, commons-conservation, international-regime-deferral) yield structurally distinct ε values, beneficiary/victim sets, and classifications, and cannot be merged into one constraint with an observable parameter. commons_conservation reads the same textual provision as barring private extraction entirely (much lower ε, near-mountain or rope depending on enforcement strength); international_regime reads it as deferring the question (scaffold-shaped, pending regime formation). This extraction_permissive story's ε (0.72) reflects only the world in which this specific reading is the operative one, as currently realized through US/Luxembourg/UAE/Japan domestic legislation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
