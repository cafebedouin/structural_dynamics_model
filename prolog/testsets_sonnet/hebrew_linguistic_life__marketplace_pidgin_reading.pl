% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Pre-Revival Inter-Communal Market Medium (Marketplace Pidgin Reading)
 *   domain: sociolinguistic/religious/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'Hebrew linguistic
 *   life' kernel: the marketplace pidgin reading, which holds that a language
 *   is alive when it functions as a practical inter-communal coordination
 *   medium, independent of native-speaker status or sacred/liturgical
 *   function. Under this reading, Hebrew (in a modified Medieval register)
 *   was continuously alive in Ottoman-era Jerusalem markets well before 1880,
 *   functioning as a lingua franca among Sephardi, Ashkenazi, Mizrahi, and
 *   Arabic-speaking traders and pilgrims. This is not the
 *   liturgical-preservation reading (unbroken sacred recitation) nor the
 *   native-generational reading (children's mother-tongue acquisition and
 *   full daily secular use) — those are separate sibling constraints with
 *   their own ε and stakeholder structure. This reading's distinctive claim
 *   is that functional, non-native, non-sacred use in commerce is sufficient
 *   for linguistic life; it denies that either sacred continuity or
 *   generational native acquisition is a necessary condition.
 *
 * KEY AGENTS:
 *   - jerusalem_market_traders: primary beneficiaries of the coordination function, moderate power, constrained exit (embedded in local trade)
 *   - sephardi_ashkenazi_intermediaries: agenda-setters who maintain and adapt the pidgin register pragmatically across generations
 *   - multi_ethnic_pilgrim_communities: transient beneficiaries relying on the register situationally
 *   - revivalist_historiographers: institutional payers whose founding myth of linguistic resurrection is costly to sustain if this reading is accepted
 *   - pure_vernacular_nativist_advocates: payers who lose exclusive definitional authority over what counts as linguistic life
 *   - comparative_sociolinguists: analytical observers assessing the pidgin against typological standards for trade lingua francas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.22).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Pre-Revival Inter-Communal Market Medium (Marketplace Pidgin Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistic/religious/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e').
narrative_ontology:cs_kernel_codification('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', distributed).
narrative_ontology:cs_authority_grounding('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', distributed).
narrative_ontology:cs_reading_relation('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', foundational, functional_coordination_suffices_for_vitality).
narrative_ontology:cs_axiom_status(functional_coordination_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', functional_coordination_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', secondary, native_acquisition_not_necessary_for_life).
narrative_ontology:cs_axiom_status(native_acquisition_not_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', native_acquisition_not_necessary_for_life, empirically_contingent).
narrative_ontology:cs_reference_frame('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', ottoman_jerusalem_mixed_market_register).
narrative_ontology:cs_drift_state('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', post_zionist_revival_historiography, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c5060e0a-6d58-4c3f-9ddc-60d4d7fd846e', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_market_traders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, multi_ethnic_pilgrim_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intermediaries).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, revivalist_historiographers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, pure_vernacular_nativist_advocates).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, language_vitality_independent_of_native_acquisition).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, continuous_adaptation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sephardi, Ashkenazi, Mizrahi, and Arabic-speaking traders in pre-1880 Jerusalem markets use a modified Medieval Hebrew register as a working lingua franca for pricing, contracts, and disputes across mutually unintelligible home vernaculars (Ladino, Yiddish, Judeo-Arabic). They did not need to be native speakers to depend on it daily; the pidgin solved a live coordination problem at the point of exchange.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_market_traders, beneficiary,
    moderate, biographical, constrained, local).

% Pilgrims and itinerant Jews arriving from disparate diaspora communities rely on a shared, simplified Hebrew register to transact and communicate with residents whose home language they do not share; use is functional and situational, dropped upon return home, but genuinely load-bearing while present.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, multi_ethnic_pilgrim_communities, beneficiary,
    moderate, immediate, mobile, regional).

% Community brokers, scribes, and market officials maintain and lightly standardize the pidgin register across generations to keep inter-communal trade functioning; they are not linguistic purists and adapt vocabulary and syntax pragmatically as trade needs shift, without appeal to sacred or national justification.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intermediaries, agenda_setter,
    moderate, generational, constrained, local).

% Zionist linguistic revival narratives (post-1880, associated with Ben-Yehuda) depend on Hebrew having been effectively dead as a spoken vernacular before deliberate revival. Evidence of a continuously functioning pre-1880 pidgin undercuts the 'resurrection from the dead' founding myth and forces a costly historiographic revision; this reading is a genuine cost to that narrative, not a neutral fact for it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, revivalist_historiographers, payer,
    institutional, generational, constrained, national).

% Advocates of the native-generational standard (life = mother-tongue acquisition by children) lose their exclusive claim on what counts as 'alive' if this reading is accepted, since it would mean Hebrew was already alive by a different, lower-bar standard prior to any revival of child acquisition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, pure_vernacular_nativist_advocates, payer,
    moderate, generational, constrained, national).

% Study the Jerusalem pidgin alongside other trade lingua francas (Sabir, Chinook Jargon, market pidgins generally) to assess whether functional inter-communal use, independent of native-speaker status, constitutes linguistic vitality by comparative typological standards.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, comparative_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared functional register enabling trade, contracts, and daily transaction among mutually unintelligible Jewish diaspora vernaculars and other regional languages in a mixed marketplace, without requiring any party to abandon their home tongue.
% TRANSFER_FUNCTION: Moves practical communicative capacity (price agreement, contract terms, dispute resolution) across communal linguistic boundaries at the point of exchange; does not transfer wealth or status directly but enables the transactions that do.
% ABSENT_VOICES: Revivalist historiographers and nativist linguists are not absent from the conversation in the modern sense, but their founding narratives are structurally threatened by this reading and they have strong incentive to minimize or recharacterize the pidgin's vitality as merely liturgical residue rather than living coordination.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin's functional role had simply vanished before 1880, the coordination problem it solved would have been absorbed by Arabic, Ladino, or Yiddish as informal trade languages; whether the market economy of Ottoman Jerusalem would have meaningfully reorganized, or barely noticed, is disputed among historians — hence contested rather than a clean rearrangement verdict.
% FOUNDING_PROBLEM: Multiple diaspora Jewish communities and pilgrims converging in Ottoman Jerusalem needed a shared, workable medium for everyday commerce and dispute resolution that none of their mutually unintelligible home vernaculars could provide.
% FOUNDING_PROBLEM_CORROBORATION: The specific pre-1880 coordination problem (a mixed, non-native-Hebrew-speaking Jewish population needing a market lingua franca) was superseded by twentieth-century Hebrew revival producing a native-speaker population and by broader adoption of Arabic and later Modern Hebrew as unmarked vernaculars; this is attested by outside comparative sociolinguists studying trade-pidgin lifecycles generally, not solely by parties invested in either the revivalist or pidgin narrative.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.28 by 1880) because the pidgin's function is genuinely coordinative — it solves a real cross-communal trade problem without concentrated rent extraction; the modest upward drift reflects gradual formalization and standardization pressure as trade volume grew, not predation. Suppression is low (0.22): no coercive apparatus forces traders to use the register; they adopt it because it works. Theater ratio stays low throughout (0.08 to 0.15): the register's use tracks real transactional need rather than performative or ideological display. Accessibility collapse (0.35) is moderate rather than high, consistent with a genuine rope: alternative trade languages (Arabic, Ladino informally) remained available; the pidgin was preferred for efficiency, not because alternatives were suppressed. Resistance (0.4) reflects the real historiographic and definitional resistance this reading meets from revivalist and nativist camps, not resistance from the market participants who used the pidgin, who had no reason to resist a tool that worked for them.
 *
 * DIRECTIONALITY LOGIC:
 *   Market traders and pilgrim communities sit near the beneficiary end: the pidgin subsidizes their transactional capacity at low cost, and they can exit to other registers (Arabic, home vernaculars) without catastrophic loss, so directionality stays moderate-low. The intermediaries who maintain the register are structurally positioned between beneficiary and agenda-setter — they gain communal standing from brokering the register but bear the generational labor of adapting it. Revivalist historiographers and nativist advocates are payers in a definitional sense, not an economic one: this reading costs them narrative and disciplinary authority, which is real but not extractive in the same way as material rent — hence their d is elevated by narrative stakes even though no material transfer runs to the beneficiary group.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a rope (not a mountain, not a snare) prevents two mislabelings: first, it resists treating this functional pidgin as a pure natural fact requiring no coordination effort (a mountain framing would erase the intermediaries' active maintenance work); second, it resists treating the sibling revivalist narrative's discomfort as evidence that this reading is itself extractive of the revivalist camp — the discomfort is reputational/historiographic, not a material transfer this constraint enacts. The founding problem is correctly marked 'dead' by 1880-2020 standards (the specific coordination gap the pidgin filled no longer exists in that form) while the reading itself remains contested precisely because its historical reality bears on present-day national narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vitality_threshold_ambiguity,
    'Does documented functional use of a modified Hebrew register in pre-1880 Jerusalem markets meet a defensible cross-linguistic threshold for ''linguistic life,'' or is it better characterized as a specialized register/jargon insufficient to count as the language being alive in the sense later claimed by revival narratives?',
    'Comparative corpus analysis against established trade-pidgin and lingua-franca typologies (e.g., Sabir, Chinook Jargon, medieval Mediterranean lingua francas) to determine whether the documented usage patterns meet standard sociolinguistic vitality criteria independent of native-speaker status.',
    'If the threshold is met, this reading is empirically well-grounded and the liturgical-only and pure-revival narratives require substantial revision. If not met, this reading overclaims and the pidgin should be recharacterized as a narrower functional register rather than evidence of full linguistic life.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_vitality_threshold_ambiguity, empirical, 'Whether documented marketplace usage meets a defensible vitality threshold.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three sibling readings of ''Hebrew linguistic life'' (liturgical, marketplace-pidgin, native-generational) genuinely independent structural claims, or do they secretly share enough conceptual overlap that a single unified definition of linguistic vitality could subsume all three without loss?',
    'Formal comparison of the three readings'' necessary-and-sufficient conditions against a broader corpus of sociolinguistic vitality frameworks to test whether any single framework can jointly satisfy all three without contradiction.',
    'If genuinely independent, the three-way decomposition into separate constraint stories is the correct authoring choice (per the ε-invariance principle) and each carries distinct policy/historiographic stakes. If subsumable, the decomposition may overstate structural distinctness for what is really one contested continuum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are structurally independent or secretly unifiable.').

omega_variable(
    revivalist_narrative_material_stakes,
    'Does acceptance of the marketplace pidgin reading carry material stakes (funding, institutional legitimacy, national foundational mythology) for revivalist historiographers beyond pure scholarly disagreement, making their resistance partly self-interested rather than purely evidentiary?',
    'Institutional and funding-history analysis of Hebrew revival historiography programs, examining whether resistance to pre-1880 vitality evidence correlates with institutional or national-narrative investment.',
    'If material stakes are substantial, the ''payer'' framing for revivalist historiographers is a defensible directionality classification with real institutional cost; if stakes are minimal, the payer framing overstates the cost this reading imposes on that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revivalist_narrative_material_stakes, conceptual, 'Whether revivalist resistance to this reading is materially interested or purely evidentiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1750, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement(hebr_tr_t1776, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1776, 0.09).
narrative_ontology:measurement(hebr_tr_t1802, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1802, 0.1).
narrative_ontology:measurement(hebr_tr_t1828, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1828, 0.12).
narrative_ontology:measurement(hebr_tr_t1854, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1854, 0.13).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1750, 0.18).
narrative_ontology:measurement(hebr_be_t1776, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1776, 0.2).
narrative_ontology:measurement(hebr_be_t1802, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1802, 0.22).
narrative_ontology:measurement(hebr_be_t1828, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1828, 0.24).
narrative_ontology:measurement(hebr_be_t1854, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1854, 0.26).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_linguistic_life__marketplace_pidgin_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.03).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_linguistic_life kernel, decomposed per the ε-invariance principle because the natural-language question 'was Hebrew alive before the Zionist revival?' conflates three structurally distinct vitality criteria (liturgical continuity, functional inter-communal coordination, native generational acquisition) with different ε values, different beneficiary/victim sets, and different classification outcomes. This story (marketplace_pidgin_reading) has the lowest extraction/suppression profile of the three, consistent with describing genuine low-friction coordination rather than a contested authority claim. The liturgical_preservation_reading and native_generational_reading siblings should be authored as separate constraint stories linked back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
