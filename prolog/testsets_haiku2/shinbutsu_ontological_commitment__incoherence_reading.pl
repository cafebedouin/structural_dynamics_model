% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Shinbutsu-shugo (kami-buddha syncretism) in the Edo period (1603–1868)
 *   was no coherent theological system. Rather, it was institutional
 *   tolerance of ontological incoherence: Shinto and Buddhism coexisted in
 *   the same practitioners' lives and in the same temple complexes, with no
 *   authoritative doctrine reconciling their metaphysical premises. This
 *   reading treats that incoherence as itself the constraint — not a failed
 *   synthesis waiting for honji-suijaku (the syncretic-reading), not a
 *   functional partition of separate domains (the partition-reading), but a
 *   stable institutional arrangement whose function was precisely to avoid
 *   requiring coherence. The Meiji state's rapid dismantling of this
 *   arrangement (shinbutsu bunri, 1870s) via state decree is evidence the
 *   incoherence was contingent institutional fact, not natural theological
 *   settlement. The reading instantiates the constraint as a Piton:
 *   functionally degraded (the original coordination problem — practical
 *   coexistence without doctrinal unity — was solved and no longer required
 *   maintenance), theatrically maintained (practical rituals continued; no
 *   one asked why the kami and buddhas were never unified), and easy to
 *   dismantle (once the Meiji state moved to enforce partition, no coherent
 *   doctrine defended the incoherent status quo).
 *
 * KEY AGENTS:
 *   - Edo period lay practitioners: absorb dual religious life as ambient fact; identity-locked into both traditions via kinship and community.
 *   - Buddhist institutional clergy: benefit from mortuary monopoly without needing a theodicy reconciling kami and buddhas; maintain authority through functional dominance, not doctrinal defense.
 *   - Shinto priesthood: maintain ritual authority over life-passage and fertility rites; insulated from doctrinal challenge by incoherence.
 *   - Meiji state apparatus: benefits from prior lack of coherent doctrine because it permits rapid, low-resistance restructuring.
 *   - Edo philosophers and scholars: recognize incoherence, propose unifying doctrines (partition or syncretic models), but remain institutionally excluded because neither religious authority has incentive to authorize constraints on their domain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.55).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, piton).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '50e3028d-cd3f-4fbb-b861-2412fee8ac44').
narrative_ontology:cs_kernel_codification('50e3028d-cd3f-4fbb-b861-2412fee8ac44', implicit).
narrative_ontology:cs_authority_grounding('50e3028d-cd3f-4fbb-b861-2412fee8ac44', distributed).
narrative_ontology:cs_reading_relation('50e3028d-cd3f-4fbb-b861-2412fee8ac44', shinbutsu_ontological_commitment__shinbutsu_syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('50e3028d-cd3f-4fbb-b861-2412fee8ac44', shinbutsu_ontological_commitment__shinbutsu_partition_reading, coexists_with).
narrative_ontology:cs_axiom('50e3028d-cd3f-4fbb-b861-2412fee8ac44', foundational, ontological_commitment_never_established).
narrative_ontology:cs_axiom_status(ontological_commitment_never_established, holdable).
narrative_ontology:cs_axiom_grounding('50e3028d-cd3f-4fbb-b861-2412fee8ac44', ontological_commitment_never_established, empirically_contingent).
narrative_ontology:cs_axiom('50e3028d-cd3f-4fbb-b861-2412fee8ac44', secondary, institutional_tolerance_enabled_practical_coexistence).
narrative_ontology:cs_axiom_status(institutional_tolerance_enabled_practical_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('50e3028d-cd3f-4fbb-b861-2412fee8ac44', institutional_tolerance_enabled_practical_coexistence, instrumental).
narrative_ontology:cs_reference_frame('50e3028d-cd3f-4fbb-b861-2412fee8ac44', edo_unsystematized_dual_practice).
narrative_ontology:cs_drift_state('50e3028d-cd3f-4fbb-b861-2412fee8ac44', meiji_state_enforcement_period, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('50e3028d-cd3f-4fbb-b861-2412fee8ac44', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shinto_priesthood).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, coherence_seeking_philosophers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutional_clergy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, edo_period_lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, edo_period_philosophers_and_scholars).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, practical_coexistence_trumps_metaphysical_coherence).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, institutional_tolerance_enables_religious_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigate a dual religious life: birth and marriage rites (Shinto), death and ancestral veneration (Buddhism), seasonal festivals (both mixed). No external authoritative voice reconciles the metaphysical premises; practitioners absorb the contradiction as 'how things are done.' To exit either tradition would sever kinship bonds and community identity. The incoherence is their ambient institutional fact, neither theorized nor systematized into a unified framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, edo_period_lay_practitioners, payer,
    powerless, biographical, identity_locked, national).

% Dominate the mortuary and ritual-service domain; collect fees from death rites, memorial services, and temple patronage. The institutional incoherence allows them to operate funerary authority without needing a theodicy for kami-buddha ontological status. As long as no systematic doctrine emerges, their functional monopoly on death-rite authority is unchallenged. They have administrative capacity to reframe doctrine if threatened (honji-suijaku availability as a unifying move, or partition into separate domains).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutional_clergy, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutional_clergy, agenda_setter).

% Maintain ritual authority over fertility, harvest, life-passage, and seasonal kami veneration. The incoherence insulates them from doctrinal competition with Buddhism: as long as no framework coherently subordinates kami to buddhas (or vice versa), their domain remains unchallenged. They possess the capacity to articulate and enforce doctrinal boundaries (as they do post-Meiji) but have no incentive to do so while the incoherence persists.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shinto_priesthood, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, shinto_priesthood, agenda_setter).

% Inherits an incoherent religious landscape and systematically dismantles it: decrees the separation of kami and buddhas ('shinbutsu bunri'), privileges Shinto as the state cult, and relegates Buddhism to private practice. The pre-existing incoherence made this separation technically easy and politically feasible — there was no defended unified doctrine to overturn. The state benefits from the prior lack of ontological commitment because it permits radical restructuring without facing a coherent alternative framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Recognize the ontological incoherence and attempt to theorize it (Kamo no Mabuchi, Hirata Atsutane on Shinto restoration; Confucian critiques of both traditions). Their proposals for coherence — whether partition-reading (separate domains), syncretic-reading (unified metaphysics), or restoration-reading (Shinto primacy) — meet institutional resistance: neither Buddhist nor Shinto authorities have incentive to authorize a doctrine that would constrain their functional autonomy. The incoherence persists because no authority cares enough to resolve it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, edo_period_philosophers_and_scholars, payer,
    moderate, biographical, constrained, national).

% Mixed Buddhist-Shinto temples and honji-suijaku-based institutional frameworks are excluded from the post-Meiji arrangement (state enforcement of separation). They held no canonical authority during the Edo incoherence (honji-suijaku was a scholarly option, not institutional doctrine), so the state's enforcement of partition dismantles them without requiring a counterargument.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, post_meiji_syncretic_institutions, excluded,
    institutional, generational, trapped, national).

% Maps the constraint structure: incoherence was institutionally tolerated (neither authorities moved to resolve it), theatrically maintained (practical accommodation + doctrinal silence = appearance of coherence), and fragile (a single determined authority could restructure it, as the Meiji state did).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, buddhist_institutional_clergy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the practical problem of dual-tradition participation: both Buddhist and Shinto rites are socially required; the incoherence permits both without requiring practitioners or institutions to defend a unified metaphysical framework that might exclude one tradition.
% TRANSFER_FUNCTION: Transfers interpretive burden from institutions to practitioners: lay people absorb the contradiction as ambient fact; institutions (both Buddhist and Shinto) collect rents (fees, patronage, social authority) without spending institutional capital on doctrine that might constrain their domain.
% ABSENT_VOICES: Coherence-seeking philosophers and scholars who proposed unifying doctrines (partition or syncretic readings). They were present in writing but institutionally excluded from authority: neither Buddhist nor Shinto authorities moved to canonize or enforce their proposals because institutional incoherence served both traditions better than any coherent doctrine would.
% DISAPPEARANCE_RATIONALE: The incoherence functionally disappeared during the Meiji period via state-imposed partition — kami and buddhas were declared separate, Shinto was elevated to state cult. Lay practitioners reorganized: some intensified Shinto practice, some Buddhism, many maintained both but with clearer boundaries. The constraint (tolerated incoherence) vanished; the world reorganized around enforced separation (a new constraint). The Meiji state's ability to impose partition so rapidly is evidence that the incoherence was institutionally contingent, not naturally inevitable: it persisted because it served institutional interests, not because alternatives were absent.
% FOUNDING_PROBLEM: Early medieval Japan adopted Buddhism alongside indigenous kami veneration; no theological mechanism unified them (unlike Hindu-Buddhist synthesis or Confucian-Shinto fusion in Korea). The practical solution was institutional tolerance: both traditions operated in parallel, each claiming authority over different life domains, with no unified metaphysical framework required.
% FOUNDING_PROBLEM_CORROBORATION: Edo period scholars (Kamo no Mabuchi, Confucian critics) attested that the founding problem — absence of a unifying doctrine — was visible and concerning. The Meiji state's rapid dismantling of the arrangement (1870s shinbutsu bunri edicts) confirms the problem was solved by state action, not by institutional evolution toward coherence. Buddhist and Shinto authorities' post-Meiji acceptance of separation (rather than defense of the incoherent status quo) demonstrates they had never invested in the incoherence as a doctrine — it was an institutional convenience they abandoned when the state changed the rules.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-high (0.68 at interval end) because both Buddhist and Shinto institutions collect rents (fees, patronage, authority) from the arrangement without bearing the cost of maintaining a coherent doctrine. Theater is the dominant feature (0.72 throughout): the constraint persists through practical accommodation and doctrinal silence, not through active enforcement of a unified framework. As coherence-seeking scholarship accumulates (especially by the mid-Edo period), the theater ratio plateaus — the arrangement requires increasingly visible avoidance of doctrinal questions rather than active maintenance. Suppression is moderate (0.55): lay practitioners are identity-locked (they cannot exit without severing kinship and community ties), so active suppression of alternatives is unnecessary; the identity-lock itself sustains the incoherence. The measurement series run on a shared time grid; suppression_requirement rises as state-building activity increases (late Edo, pre-Meiji), forcing greater effort to maintain the incoherent status quo against rising pressure for coherent state ideology.
 *
 * PERSPECTIVAL GAP:
 *   From the payer (lay practitioner) seat: the incoherence is 'how things are done'; it is experienced as natural religious pluralism, not as institutional extraction. From the beneficiary (institutional clergy) seat: the incoherence is instrumental — it permits domain authority without doctrinal constraint. From the philosopher seat: the incoherence is a problem to be solved, but the solution is structurally unavailable because institutions have no incentive to authorize it. From the Meiji state seat: the incoherence is an obstacle to state religious ideology; dismantling it is a straightforward political act. The engine computes these divergences from the power/exit axes: lay practitioners' identity-lock amplifies their effective extraction (d moves toward target), while institutional actors' arbitrage options reduce it (d moves toward beneficiary). The reading's core is that this same structural fact (ontological incoherence) appears as natural-religious-reality to participants and as institutional-contingency to the external state authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (Buddhist clergy, Shinto priesthood, Meiji state apparatus) all sit near the beneficiary end (d near 0.0): they collect rents or gain restructuring opportunities from the incoherence. Lay practitioners sit near the target end (d near 1.0): identity-locked into an incoherent arrangement that extracts compliance without offering a coherent rationale. Philosophers sit in the middle: they benefit from the practical tolerance (the incoherence permits both traditions to coexist without violent suppression), but they pay an intellectual cost (their coherence-seeking proposals are institutionally ignored). The Meiji state apparatus is the analytical outlier: from outside the Edo system, it perceives the incoherence as a constraint that can be dismantled, and it benefits from dismantling it (enabling state control of religious ideology). This directionality divergence is the kernel of the reading: from inside the Edo arrangement, the incoherence is ambient and unsystematized; from the Meiji perspective, it was a contingent institutional fact ripe for restructuring.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — absence of a unifying doctrine for dual-tradition practice — was solved by the Meiji period (state-enforced partition provides a unified ideology, whether practitioners accept it or not). The constraint persists post-founding-problem via institutional inertia: Buddhist institutions continue to collect mortuary rents, Shinto priesthood maintains ritual authority, and lay practitioners continue dual practice (now with state-enforced clearer boundaries). Theater and suppression both increase as the founding problem ages because the constraint's functional justification (practical coexistence without doctrinal unity) is no longer offered — it is simply institutional fact. The Meiji dismantling (shinbutsu bunri) did not require a counterargument because the incoherence had no coherent defender; it was replaced by state-enforced partition, another institutional arrangement whose justification was ideological (Shinto primacy) rather than practical coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_unstated_synthesis,
    'Was the absence of a unified ontological doctrine due to institutional tolerance of genuine incoherence, or to an unstated practical synthesis that scholars simply did not theorize?',
    'Textual analysis of Edo philosophical discourse (Kamo no Mabuchi, Hirata Atsutane, Buddhist scholiasts) and institutional records (temple documents, priesthood correspondence) to establish whether coherence was considered but rejected, never proposed, or implicitly assumed without explicit formulation.',
    'If unstated synthesis existed, the constraint is better classified as syncretic-reading (coherence was achieved but not theorized); if genuine incoherence was tolerated, the piton classification holds (institutional maintenance of contradiction, not unifying doctrine). The measurement of theater_ratio would shift downward if synthesis were implicit (less theatrical maintenance required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_unstated_synthesis, empirical, 'Whether the constraint represents genuine ontological incoherence or unstated practical synthesis.').

omega_variable(
    identity_lock_vs_voluntary_tolerance,
    'Did lay practitioners experience the dual-tradition requirement as an identity-fused obligation, or as a pragmatic choice among available options?',
    'Post-Meiji adoption patterns: if practitioners rapidly switched to single-tradition practice (Buddhism-only or Shinto-only) where state ideology permitted, it indicates prior identity-lock; if dual practice persisted even post-prohibition of institutional syncretic forms (Buddhist-Shinto shared temples), it indicates voluntariness. Anecdotal evidence from conversion accounts and practice journals.',
    'High identity-lock (true incoherence reading) implies suppression was structural and internalized; lower lock (voluntary tolerance reading) implies suppression was weaker. If identity-lock was lower, effective extraction would be lower (d moves toward beneficiary for lay practitioners), and the constraint would compute as weaker piton or degraded rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_tolerance, empirical, 'Mechanism of practitioners'' commitment to dual practice: identity fusion vs. pragmatic choice.').

omega_variable(
    reading_foreclosure_via_meiji_state,
    'Did the Meiji state''s enforcement of partition logically foreclose the syncretic-reading and incoherence-reading as viable institutional frameworks, or did it merely make them politically unavailable?',
    'Post-Meiji persistence: syncretic practices continued in folk religion and some rural contexts despite state prohibition; incoherence persists in lay practice (both rites used without coherence) despite state claim of separation. This persistence tests whether the readings remain logically available despite state suppression.',
    'If syncretic and incoherence readings persist post-Meiji despite state interdiction, they are coexisting with the partition-reading (coexists_with relation holds), not foreclosed. If they vanish, the state enforcement achieved logical foreclosure in addition to political suppression (forecloses relation is appropriate). The cs_structure.reading_relations declaration assumes coexists_with; this omega tests whether that assumption holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_meiji_state, empirical, 'Whether the Meiji state''s partition enforcement logically forecloses sibling readings or merely suppresses their institutional expression.').

omega_variable(
    piton_vs_snare_beneficiary_concentration,
    'Who receives the extraction from the incoherent arrangement? Concentrated (Buddhist and Shinto institutions) or diffuse (lay practitioners maintain its ambiguity for their own pragmatic reasons)?',
    'Evidence of institutional capture of the incoherence (clergy actively maintain doctrinal silence to protect their domains) vs. lay preference for it (practitioners resist coherent doctrinal reforms when proposed). If Buddhist and Shinto institutions blocked coherence-seeking doctrines (evidence of capture), piton classification is correct (beneficiary concentration, institutional maintenance via theater). If lay practitioners resisted coherence and preferred ambiguity (evidence of distributed support), it''s a weak snare or degraded rope (distributed benefit, less theatrical maintenance required).',
    'Beneficiary concentration determines piton vs. snare classification. If concentrated, theater_ratio should be high (institutional performance of tolerance rather than functional coordination) — the authored 0.72 value assumes concentration. If distributed, theater_ratio should be lower (genuine practical tolerance, not performative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_snare_beneficiary_concentration, empirical, 'Concentration of extraction: institutional monopoly on incoherence management vs. distributed lay preference for ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(shin_tr_t40, observed).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 80, 0.7).
narrative_ontology:measurement_basis(shin_tr_t80, observed).
narrative_ontology:measurement(shin_tr_t120, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 120, 0.72).
narrative_ontology:measurement_basis(shin_tr_t120, observed).
narrative_ontology:measurement(shin_tr_t160, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 160, 0.72).
narrative_ontology:measurement_basis(shin_tr_t160, observed).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 200, 0.72).
narrative_ontology:measurement_basis(shin_tr_t200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(shin_be_t40, observed).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(shin_be_t80, observed).
narrative_ontology:measurement(shin_be_t120, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 120, 0.63).
narrative_ontology:measurement_basis(shin_be_t120, observed).
narrative_ontology:measurement(shin_be_t160, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 160, 0.66).
narrative_ontology:measurement_basis(shin_be_t160, observed).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement_basis(shin_be_t200, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement_basis(shin_su_t40, observed).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement_basis(shin_su_t80, observed).
narrative_ontology:measurement(shin_su_t120, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(shin_su_t120, observed).
narrative_ontology:measurement(shin_su_t160, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 160, 0.54).
narrative_ontology:measurement_basis(shin_su_t160, observed).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement_basis(shin_su_t200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_syncretic_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_partition_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu-shugo kernel splits into three constraint stories: incoherence-reading (this file — no unified doctrine, institutional tolerance of contradiction), syncretic-reading (honji-suijaku metaphysics as coherence mechanism), partition-reading (separate ontological domains). The three readings are not measurements of a single constraint from different seats; they are structurally distinct constraints arising from the same kernel, with different ε values, different beneficiary/victim structures, and different predicted Meiji-period outcomes. The incoherence-reading predicts easy dismantling because no coherent doctrine defends it; the syncretic-reading predicts institutional collapse because honji-suijaku had no canonical defender; the partition-reading predicts it becomes the new constraint (state-enforced separation). Network edges link family members: incoherence influences both siblings (by providing the prior condition that makes both available as alternatives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
