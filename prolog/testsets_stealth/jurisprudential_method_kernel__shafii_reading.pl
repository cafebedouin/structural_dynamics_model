% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Four-Tier Source Hierarchy with Isnad Arbitration
 *   domain: religious/legal/institutional-history
 *
 * SUMMARY:
 *   Between roughly 800 and 1040 CE, al-Shafi'i's methodological framework —
 *   a strict four-tier hierarchy of Qur'an, authenticated Hadith, juristic
 *   consensus (ijma'), and regulated analogy (qiyas) — converted Islamic
 *   legal derivation from a set of regional practices into a single ranked
 *   procedure, with verified hadith transmission installed as the arbiter of
 *   what counts as prophetic precedent. The framework solved a real
 *   fragmentation problem: before it, Medinan practice, Iraqi analogy, and
 *   local custom operated as parallel authorities with no agreed procedure
 *   for conflict. The same structure concentrated legal authority in whoever
 *   controlled isnad authentication — a scarce, credentialed skill — and
 *   stripped customary practice and independent reasoning of source-status.
 *   This file instantiates ONE reading of the jurisprudential_method_kernel
 *   (the shafii_reading); the hanafi, maliki, and hanbali readings are
 *   separate constraints with their own epsilon values and beneficiary
 *   structures, linked through the network section. Claim and metrics are
 *   authored independently: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination plus asymmetric extraction) and the metrics
 *   describe moderately-high extraction rising across the interval as
 *   credentialing hardened into the taqlid-era settlement.
 *
 * KEY AGENTS:
 *   - hadith_scholars (muhaddithun): primary beneficiary (organized/identity_locked) — the isnad craft becomes the sole arbiter of prophetic precedent
 *   - shafii_madhhab_establishment: agenda-setter and institutional collector (institutional/constrained) — teaches, administers, licenses, and staffs the hierarchy
 *   - customary_practice_communities: primary target (powerless/trapped) — local law loses source-status unless reducible to authenticated reports
 *   - analogical_jurists_ahl_al_ray: secondary target (organized/constrained) — analogy demoted to last resort, preference banned
 *   - medinan_practice_community: secondary target (organized/constrained) — living practice subordinated to chained reports
 *   - oral_custom_holders: excluded voice (powerless/trapped) — legal knowledge without transmissible chains has no seat in the framework
 *   - fiqh_historians: analytical observer (analytical/analytical) — sees the full structure including what the standardization displaced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.4).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Four-Tier Source Hierarchy with Isnad Arbitration").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "religious/legal/institutional-history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c').
narrative_ontology:cs_kernel_codification('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', fixed_text).
narrative_ontology:cs_authority_grounding('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', lineage).
narrative_ontology:cs_interpretation_layer_present('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c').
narrative_ontology:cs_reading_relation('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', foundational, isnad_transmission_arbitrates_source_conflict).
narrative_ontology:cs_axiom_status(isnad_transmission_arbitrates_source_conflict, holdable).
narrative_ontology:cs_axiom_grounding('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', isnad_transmission_arbitrates_source_conflict, instrumental).
narrative_ontology:cs_axiom('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', foundational, four_tier_exhaustive_ranked_sources).
narrative_ontology:cs_axiom_status(four_tier_exhaustive_ranked_sources, holdable).
narrative_ontology:cs_axiom_grounding('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', four_tier_exhaustive_ranked_sources, theological).
narrative_ontology:cs_reference_frame('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', isnad_arbitrated_textual_hierarchy).
narrative_ontology:cs_drift_state('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', classical_taqlid_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9bc7de7f-494f-4b4c-ab8c-a4a37cb7a42c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_madhhab_establishment).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_jurists_ahl_al_ray).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, medinan_practice_community).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, prophetic_sunna_supremacy_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, isnad_reliability_premise).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, textual_source_ranking).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Specialists in isnad criticism — the craft of verifying transmission chains. Under the hierarchy their skill becomes the sole arbiter of what counts as prophetic precedent, converting a technical specialty into the gateway of legal authority. Their status, livelihood, and the entire value of a lifetime's training depend on the hierarchy remaining in force; exit would strand their accumulated technical capital.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary,
    organized, generational, identity_locked, continental).

% Teaches the methodology in madrasas, writes the authoritative commentaries, examines and licenses jurists, staffs judgeships, and controls curricula and appointments. Administers the ranked procedure day to day and collects endowment revenue, student fees, and positional authority through it. Leaving would mean abandoning institutional position built entirely on the framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_madhhab_establishment, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, shafii_madhhab_establishment, beneficiary).

% Communities whose inherited local law operated for generations without transmissible chains. Under the hierarchy their practice has no standing unless reducible to authenticated reports — reports identified by specialists they do not control. They bear the cost of having their law re-derived through texts and procedures owned by others; emigration or communal dissolution are the only exits and both carry severe costs.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_communities, payer,
    powerless, generational, trapped, regional).

% Jurists trained in extensive analogy and juristic preference, organized in school networks with their own teaching lines. The hierarchy demotes their tools to a last-resort fourth tier and bans preference outright as unauthorized legislation. They retain a licensed residue of regulated analogy but lose independent authority; exit means joining another school or fighting inside this one.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_jurists_ahl_al_ray, payer,
    organized, biographical, constrained, continental).

% Heirs of the oldest continuous practice tradition, claiming embodied communal preservation of prophetic practice. The hierarchy subordinates their living practice to individually chained reports, requiring them to re-present practice as text or lose it. Their prestige as the Prophet's city persists, but their source-status is gone; their organized voice bought argumentative accommodation, not procedural standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, medinan_practice_community, payer,
    organized, generational, constrained, regional).

% Holders of legal knowledge in memory and practice — elders, women managing household and family law, local arbitrators — who cannot produce transmission chains. The methodology's construction never included them: the debate over legal sources occurred entirely among literate transmission specialists. Their law remains invisible to the framework that governs them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, oral_custom_holders, excluded,
    powerless, generational, trapped, local).

% Reconstruct the formation of legal methodology from surviving polemics, manuscripts, and school records. They see the whole structure, including what the standardization displaced and which claims about the founding crisis each school's origin narrative serves. They hold no stake in the framework's maintenance.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, fiqh_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, shafii_madhhab_establishment).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves source-fragmentation: provides a single ranked procedure for resolving conflicts among revealed and transmitted materials, making rulings derivable, teachable, examinable, and portable across a continental legal community instead of varying by region and teacher.
% TRANSFER_FUNCTION: Moves legal authority — and the status, appointments, and endowment income riding on it — toward possessors of authenticated transmission, away from holders of living or customary practice and from reasoners whose tools lack textual anchors.
% ABSENT_VOICES: Oral custom holders, lay practitioners, and the women whose household law governed most daily life had no seat in the methodology's construction; the founding debate ran entirely among literate transmission specialists, so unanimity about what counts as a legal source arose in a room those seats never entered.
% DISAPPEARANCE_RATIONALE: If the hierarchy vanished overnight, legal authority would flow back to whatever each region's practice and reasoning traditions could carry: school boundaries would redraw around source-theories rather than around transmission credentials, madrasa curricula and licensing would lose their spine, and every ruling resting on authentication precedence would need re-derivation. The legal order rearranges around the constraint's absence.
% FOUNDING_PROBLEM: Pre-methodological Islamic law contained irreconcilable regional claims about prophetic precedent — Medinan practice, Iraqi analogy, local custom — with no agreed procedure for deciding which authority governs when they conflict.
% FOUNDING_PROBLEM_CORROBORATION: The rival schools' own surviving polemics corroborate that the founding crisis was real: Maliki defenses of Medinan practice and Hanafi defenses of juristic preference argue about arbitration procedure, which presupposes the conflict. Historians of Islamic law, writing from outside every benefiting party, corroborate the fragmentation while disputing the status question — traditionalist seats hold the problem permanently live because novel cases never cease, historian seats hold the acute founding crisis dead and the surviving arrangement partly self-perpetuating. Corroboration for the problem's existence is external; corroboration for its current status is split along beneficiary lines.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.68 at interval end) is medium-high: the credentialing gate decouples legal authority from service rendered — a ruling's validity comes to depend on authentication credentials rather than the outcome's quality — and strips rival sources of standing, transferring authority to credential-holders. It stops short of snare level because the arbitration service is real: isnad criticism did filter fabrication, and the ranked procedure did make rulings derivable and portable. Suppression (0.40 current) is structural-institutional rather than coercive: rival schools were never outlawed, but within the framework dissent carried curricular, examination, and appointment costs. Theater (0.38) rises across the interval as live arbitration gives way to taqlid-era reproduction — commentary-writing and license-granting increasingly perform adherence to a method whose critical edge is dormant. Accessibility_collapse (0.48): within the framework alternatives collapse almost completely (once transmission is the arbiter, custom and preference have no independent standing), but cross-framework alternatives — the other three schools — persisted throughout, holding the global figure moderate. Resistance (0.58): sustained inter-school contestation for roughly two centuries, Maliki defenses of practice and Hanafi defenses of reasoning, ending in accommodation rather than capitulation. All three metric series share one grid (0/40/80/120/160/200/240). The suppression_requirement series is deliberately DECLINING: enforcement capacity migrated from costly polemic (al-Shafi'i arguing personally against istihsan and against Medinan practice overriding authentic reports) into self-sustaining curricular structure — enforcement decay through success, not liberalization. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith_scholars seat the hierarchy is the triumph of rigor: fabrication filtered, divine intent made accessible, a vocation dignified — and identity_locked exit means the framework's fall would strand a lifetime's technical capital, so the seat experiences any challenge as existential attack. From the customary_practice_communities seat the same structure is dispossession: law they lived becomes law they must cite, in a vocabulary they do not control. The two organized payer seats diverge despite comparable nominal standing: ahl_al_ray retained a continental school network and a licensed analogy residue (constrained exit, partial mitigation), while the medinan community held regional prestige without a procedural role (constrained exit, no mitigation) — the engine computes different effective extraction for the two from these exit differences. The establishment seat experiences the constraint as the thing it IS: institutional identity fused with the methodology, so administration and belief are indistinguishable from inside.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. hadith_scholars (declared beneficiary, identity_locked) derive near the full-beneficiary end: the constraint subsidizes their craft, and identity lock deepens rather than dampens the subsidy. shafii_madhhab_establishment (agenda_setter with secondary beneficiary role, constrained) sits similarly low: it administers the structure that pays it. customary_practice_communities (victim, trapped, powerless) derive near the full-target end — no exit, no countervailing benefit. medinan_practice_community (victim, organized, constrained) derives high but below the trapped seats: organized voice purchased argumentative accommodation without restoring source-status. analogical_jurists_ahl_al_ray (victim, organized, constrained) derives high-but-not-maximal: the framework demoted their tools yet licensed a fourth-tier analogy residue, a partial in-framework benefit that the raw victim declaration alone would miss; this nuance is recorded here rather than as a directionality override because it modulates, not reverses, the derived value. Continental spatial scope raises verification difficulty, modestly amplifying effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against both mislabels. Reading the framework as pure rope would erase the credentialing transfer — authority moved to transmission specialists and away from practice-communities through the same structure that solved fragmentation. Reading it as snare would erase the genuine coordination achievement — the pre-methodological fragmentation was real, attested by all four schools' own origin narratives, and the ranked procedure demonstrably enabled a continental legal order. On mandatrophy: the founding problem (arbitrating conflicting claims of prophetic precedent) is authored contested — traditionalist seats hold it permanently live because novel cases never cease; historian seats hold the acute founding crisis dead and the surviving arrangement partly self-perpetuating. founding_problem_status=contested crossed with disappearance_verdict=world_rearranges raises no zombie flag, correctly: the theater_ratio rise (0.12 to 0.38) signals aging, not death — the methodology remains taught, cited, and applied wherever the framework governs. mandatrophy_resolved is therefore NOT declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the shafii_reading of jurisprudential_method_kernel; how would the classification shift if authored from the hanafi, maliki, or hanbali seat?',
    'Generate the three sibling stories and compare epsilon, beneficiary sets, and computed types across the family; the divergence locates the reading-indexed component of every value.',
    'Under maliki_reading the beneficiary set shifts toward Medinan practice-keepers and customary practice flips from victim to vindicated source; under hanbali_reading analogy loses even fourth-tier standing, raising suppression of reasoning-based practice; under hanafi_reading the credentialing gate loosens and epsilon likely falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame indexicality: one kernel, four readings, four constraints.').

omega_variable(
    non_textual_source_status,
    'Where the four readings disagree is the source-status of non-transmitted materials: does living practice or unanchored reasoning constitute law, or merely transmit it?',
    'Close reading of each school''s treatment of Medinan practice and juristic preference in conflict cases; the structural test is whether the material can override an authenticated solitary report.',
    'If non-transmitted materials have zero source-status, the shafii extraction story is maximal; if they retain residual status through mass-transmission arguments, part of the measured extraction is re-description rather than dispossession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_textual_source_status, conceptual, 'The located disagreement axis of the kernel.').

omega_variable(
    authentication_self_reference,
    'Does isnad authentication track historical proximity to prophetic practice, or does the authentication apparatus confer the authority it purports to discover — forged chains laundering invented texts?',
    'Text-criticism studies of hadith corpora, corpus dating, and analysis of chain-convergence patterns against independent historical records.',
    'If authentication is substantially self-referential, epsilon rises — the arbiter collects certification rents on goods it partly manufactures — and the coordination-function half of the tangled_rope claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_self_reference, empirical, 'Reliability of the constraint''s arbiter mechanism.').

omega_variable(
    coordination_extraction_separability,
    'Is the fragmentation-solving value of a unified legal methodology separable from the credentialing gate that concentrates authority in transmission specialists?',
    'Compare systemic coherence in schools retaining non-textual sources (Maliki practice) against strict-hierarchy schools across matched periods and case domains.',
    'If separable, the gate is extractive overlay removable without losing coordination; if inseparable, part of measured epsilon is the price of the coordination itself and the rope component is larger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the tangled_rope''s two components can be pried apart.').

omega_variable(
    taqlid_theater_trajectory,
    'Is the rising theater_ratio driven by contingent closure-of-ijtihad politics or by unavoidable institutional aging of any methodological regime?',
    'Correlate theater indicators with periods that honored fresh independent-jurisdiction claims (reform movements, later revivalist eras) versus rote-reproduction periods.',
    'Contingent drivers imply scaffold-like remediation (reopening independent jurisdiction restores function); inherent aging implies a piton trajectory the corpus should watch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_theater_trajectory, empirical, 'Cause attribution for the theater_ratio drift.').

omega_variable(
    kernel_framing_underdetermination,
    'Is fixed_text with lineage authority the only defensible commitment-system framing, or does a meta-kernel framing (the methodology itself as an implicit kernel grounded in the jurists'' practice) fit equally well and classify differently?',
    'Test whether the framework''s authority survives erosion of the texts'' practical fixity; if authority tracked the method rather than the text, the implicit-kernel framing fits better.',
    'Under the meta-kernel framing, drift reads as codification_collapse rather than practice_drift and the authority structure looks self-grounding — changing the commitment-system pattern classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination routed through the omega infrastructure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jmk_shafii_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jmk_shafii_tr_t0, observed).
narrative_ontology:measurement(jmk_shafii_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(jmk_shafii_tr_t40, observed).
narrative_ontology:measurement(jmk_shafii_tr_t80, jurisprudential_method_kernel__shafii_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement_basis(jmk_shafii_tr_t80, observed).
narrative_ontology:measurement(jmk_shafii_tr_t120, jurisprudential_method_kernel__shafii_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement_basis(jmk_shafii_tr_t120, observed).
narrative_ontology:measurement(jmk_shafii_tr_t160, jurisprudential_method_kernel__shafii_reading, theater_ratio, 160, 0.27).
narrative_ontology:measurement_basis(jmk_shafii_tr_t160, observed).
narrative_ontology:measurement(jmk_shafii_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.33).
narrative_ontology:measurement_basis(jmk_shafii_tr_t200, observed).
narrative_ontology:measurement(jmk_shafii_tr_t240, jurisprudential_method_kernel__shafii_reading, theater_ratio, 240, 0.38).
narrative_ontology:measurement_basis(jmk_shafii_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(jmk_shafii_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jmk_shafii_be_t0, observed).
narrative_ontology:measurement(jmk_shafii_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(jmk_shafii_be_t40, observed).
narrative_ontology:measurement(jmk_shafii_be_t80, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(jmk_shafii_be_t80, observed).
narrative_ontology:measurement(jmk_shafii_be_t120, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement_basis(jmk_shafii_be_t120, observed).
narrative_ontology:measurement(jmk_shafii_be_t160, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 160, 0.63).
narrative_ontology:measurement_basis(jmk_shafii_be_t160, observed).
narrative_ontology:measurement(jmk_shafii_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.66).
narrative_ontology:measurement_basis(jmk_shafii_be_t200, observed).
narrative_ontology:measurement(jmk_shafii_be_t240, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 240, 0.68).
narrative_ontology:measurement_basis(jmk_shafii_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(jmk_shafii_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(jmk_shafii_su_t0, observed).
narrative_ontology:measurement(jmk_shafii_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(jmk_shafii_su_t40, observed).
narrative_ontology:measurement(jmk_shafii_su_t80, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(jmk_shafii_su_t80, observed).
narrative_ontology:measurement(jmk_shafii_su_t120, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(jmk_shafii_su_t120, observed).
narrative_ontology:measurement(jmk_shafii_su_t160, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 160, 0.47).
narrative_ontology:measurement_basis(jmk_shafii_su_t160, observed).
narrative_ontology:measurement(jmk_shafii_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.43).
narrative_ontology:measurement_basis(jmk_shafii_su_t200, observed).
narrative_ontology:measurement(jmk_shafii_su_t240, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 240, 0.4).
narrative_ontology:measurement_basis(jmk_shafii_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic legal methodology' decomposes, per the epsilon-invariance principle, into four structurally distinct readings of one kernel — hanafi, maliki, hanbali, and this shafii reading. Each carries its own epsilon, beneficiary set, and victim set; forcing them into one story would average away exactly the structural differences the corpus exists to measure. Upstream/downstream texture: the shafii transmission-arbiter premise is cited as evidence inside hanbali textualist arguments (shared text-first instinct) and attacked head-on by maliki tradition-preservation arguments (embodied practice versus chained report as the access route to prophetic precedent), so this reading both supplies and contests legitimacy conditions for its siblings without resolving the family dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
