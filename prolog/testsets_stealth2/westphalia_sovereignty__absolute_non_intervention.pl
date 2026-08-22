% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Norm (Categorical Territorial Inviolability)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   A categorical rule of international order: whatever a state does inside
 *   its borders, no outside power may interfere — interference is
 *   illegitimate per se, not proportioned to the character of the internal
 *   conduct. Codified in the UN Charter's domestic-jurisdiction clause and
 *   defended through veto-gated enforcement, bloc diplomacy, and recognition
 *   practice, the rule solved the oldest collective-action problem of the
 *   state system (every state's internal arrangement was every other state's
 *   casus belli) while pricing protection out of reach for the people of
 *   predatory states, whose recourse is assigned to the very government that
 *   preys on them. This file instantiates ONE reading of the
 *   westphalia_sovereignty kernel — the absolute_non_intervention reading —
 *   as a clean, epsilon-invariant constraint; the conditional_responsibility
 *   and graded_sovereignty readings are separate files with their own epsilon
 *   values, beneficiary sets, and classifications, linked through
 *   network.affects_constraints. Epsilon's referent here is the standing
 *   categorical arrangement itself, assessed as it operates. KEY AGENTS (by
 *   structural relationship): - security_council_permanent_members:
 *   Agenda-setting gatekeeper (institutional/arbitrage) — decides when the
 *   rule yields to enforcement and collects the same shield it administers -
 *   authoritarian_regime_elites: Primary beneficiary
 *   (organized/identity_locked) — collects impunity; the rule forecloses
 *   external response to their domestic conduct - nonaligned_diplomatic_bloc:
 *   Secondary beneficiary (organized/constrained) — trades votes to keep
 *   intervention authorization blocked - liberal_democratic_governments:
 *   Dual-positioned payer-beneficiary (powerful/constrained) — pays in
 *   foreclosed humanitarian options, collects reciprocal shield -
 *   populations_under_authoritarian_rule: Primary target (powerless/trapped)
 *   — bears the arrangement's costs; protection assigned to their oppressor -
 *   persecuted_domestic_minorities: Target (powerless/trapped) — atrocity
 *   proceeds behind the guarded door - human_rights_advocacy_networks:
 *   Excluded voice (organized/constrained) — drafted the rival protective
 *   formula; barred from binding decision points - international_law_jurists:
 *   Analytical observer (analytical/analytical) — records the gap between
 *   categorical text and state practice Family note: the colloquial label
 *   'Westphalian sovereignty' conflates three structurally distinct claims,
 *   decomposed per the epsilon-invariance principle. This file carries the
 *   categorical pole (epsilon 0.66: real coordination function, asymmetric
 *   shield, actively enforced). The conditional-responsibility sibling lowers
 *   the intervention barrier and expands the protected set; the
 *   graded-sovereignty sibling replaces uniform inviolability with
 *   capacity-calibrated legitimacy and concentrates costs on low-capacity
 *   states. Both contradict this reading's categorical premise within any
 *   single framework (see cs_structure.reading_relations); all three coexist
 *   as live positions across the system's factions.
 *
 * KEY AGENTS:
 *   - security_council_permanent_members: agenda-setting gatekeeper (institutional/arbitrage) — controls enforcement authorization, collects the shield it administers
 *   - authoritarian_regime_elites: primary beneficiary (organized/identity_locked) — impunity collectors, identity-fused defenders of the categorical form
 *   - nonaligned_diplomatic_bloc: secondary beneficiary (organized/constrained) — precedent-fear voters keeping conditionality language out of binding texts
 *   - liberal_democratic_governments: dual payer-beneficiary (powerful/constrained) — foreclosed rescuers who also hold the reciprocal shield
 *   - populations_under_authoritarian_rule: primary target (powerless/trapped) — the priced-out protection demand
 *   - persecuted_domestic_minorities: target (powerless/trapped) — the rule's sharpest cost incidence
 *   - human_rights_advocacy_networks: excluded voice (organized/constrained) — rival-formula authors outside the binding venue
 *   - international_law_jurists: analytical observer (analytical/analytical) — text-practice gap recorders
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.66).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.66).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.66).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Norm (Categorical Territorial Inviolability)").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '75587dd8-7053-47d1-bc46-ebe26d151106').
narrative_ontology:cs_kernel_codification('75587dd8-7053-47d1-bc46-ebe26d151106', fixed_text).
narrative_ontology:cs_authority_grounding('75587dd8-7053-47d1-bc46-ebe26d151106', lineage).
narrative_ontology:cs_interpretation_layer_present('75587dd8-7053-47d1-bc46-ebe26d151106').
narrative_ontology:cs_reading_relation('75587dd8-7053-47d1-bc46-ebe26d151106', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('75587dd8-7053-47d1-bc46-ebe26d151106', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('75587dd8-7053-47d1-bc46-ebe26d151106', foundational, internal_conduct_irrelevant_to_inviolability).
narrative_ontology:cs_axiom_status(internal_conduct_irrelevant_to_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('75587dd8-7053-47d1-bc46-ebe26d151106', internal_conduct_irrelevant_to_inviolability, conventional).
narrative_ontology:cs_axiom('75587dd8-7053-47d1-bc46-ebe26d151106', foundational, no_external_authority_to_judge_internal_arrangements).
narrative_ontology:cs_axiom_status(no_external_authority_to_judge_internal_arrangements, holdable).
narrative_ontology:cs_axiom_grounding('75587dd8-7053-47d1-bc46-ebe26d151106', no_external_authority_to_judge_internal_arrangements, deontological).
narrative_ontology:cs_reference_frame('75587dd8-7053-47d1-bc46-ebe26d151106', categorical_territorial_inviolability).
narrative_ontology:cs_drift_state('75587dd8-7053-47d1-bc46-ebe26d151106', contemporary_r2p_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('75587dd8-7053-47d1-bc46-ebe26d151106', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, nonaligned_diplomatic_bloc).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, persecuted_domestic_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, liberal_democratic_governments).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, liberal_democratic_governments).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, westphalian_territorial_integrity_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, un_charter_article_2_7_domestic_jurisdiction).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, sovereign_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments hold veto power over any Security Council authorization of coercive action against a member state. Each decides when the non-intervention rule yields to enforcement, and each enjoys the same shield over its own conduct, including over territories it administers or contests. When consensus fails, they may act outside the framework entirely, as several did in coalition operations never authorized by the Council.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Ruling groups whose domestic conduct — mass detention, atrocity, election subversion — would otherwise invite external response. The categorical rule forecloses that response. They lobby through diplomatic blocs and allied vetoes to keep the rule categorical, and their regime narratives fuse with the principle that no outsider may judge them: accepting conduct-based conditionality would mean accepting the tribunal.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites, beneficiary,
    organized, biographical, identity_locked, national).

% A coalition of mostly post-colonial states that defends non-intervention language in every UN forum. Many members govern adequately but fear precedent: a rule letting outsiders judge conduct today could be turned on them tomorrow. They trade votes to keep intervention dependent on unanimity they can block.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, nonaligned_diplomatic_bloc, beneficiary,
    organized, generational, constrained, continental).

% Governments facing domestic pressure to respond to foreign atrocities but bound by the categorical rule they helped codify. They pay in foreclosed humanitarian options and in credibility spent on unauthorized coalition operations; they collect the same shield over their own internal conduct and the systemic stability the rule provides.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_democratic_governments, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, liberal_democratic_governments, beneficiary).

% People living under governments that torture, starve, or massacre them. The rule assigns their protection exclusively to the state oppressing them and labels external rescue aggression. Their exits are flight to refugee camps or silence; neither reaches the arrangement's decision points.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, payer,
    powerless, biographical, trapped, national).

% Minority communities targeted by their own state's security forces. Diplomatic protest is the ceiling of what the categorical rule permits outsiders on their behalf; when protest fails, atrocity proceeds behind the closed door the rule guards.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, persecuted_domestic_minorities, payer,
    powerless, biographical, trapped, local).

% NGO coalitions and jurists who drafted the responsibility-to-protect formula and lobbied it into summit documents. They speak in General Assembly debates and publish legal arguments, but the binding decision points — Council authorization — remain closed to them; their formula operates only when the veto holders permit.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, human_rights_advocacy_networks, excluded,
    organized, generational, constrained, global).

% Legal scholars and court benches mapping how the rule evolves: which breaches become precedents, which doctrines — humanitarian intervention, responsibility to protect, responsibility while protecting — gain textual life. They record the gap between the categorical text and state practice without holding enforcement power.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_law_jurists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the universal-pretext problem: before territorial fixation, every state held standing license to intervene in every other's confessional, dynastic, or ideological arrangement, making interstate war the default response to internal difference. The categorical rule removes all such licenses at once, stabilizing recognition, border settlement, and diplomatic intercourse.
% TRANSFER_FUNCTION: Moves impunity upward and recourse away: ruling elites receive an accountability shield financed by the foreclosure of external protection for their subjects; among states it exchanges mutual forbearance — each renounces intervention in all others in exchange for the same renunciation — and it concentrates the residual discretion to override the rule in five veto-holding governments.
% ABSENT_VOICES: Populations under authoritarian control are the paradigmatic absent voices: the arrangement assigns their representation to the governments that repress them, so the people whose protection is priced out never appear at any decision point. Stateless nations and unrecognized territories are doubly absent — outside both the rule's protection and its councils. Advocacy networks that drafted the rival protective formula speak in assembly debates but are barred from the binding venue.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, every internal conflict becomes a potential interstate war: borders lose legal fixity, recognition politics collapses into capability politics, alliance systems re-arm around preemptive intervention rights, and the Charter order's remaining machinery — Chapter VII, peaceful settlement, diplomatic immunity — loses its foundation. Either universal intervention or universal war follows; neither resembles the current world.
% FOUNDING_PROBLEM: Ending doctrinally licensed cross-border warfare: after 1648 the founding problem was sovereigns waging war over each other's internal confessional arrangements; the twentieth-century codification extended it to revolutionary contagion and ideological crusade.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the confessional wars attests the original founding problem is resolved — no interstate war is fought over internal creed under the post-1648 settlement's mature operation. Outside the beneficiary set, the ICISS commission (2001), successive Secretaries-General' reports, and ICJ engagement with humanitarian-exception arguments attest that a generalized version (response to mass atrocity, transnational-threat pretexts) remains live; the G77 and the veto blocs, speaking from inside the beneficiary set, dispute this. No source outside the benefiting parties attests that the categorical form specifically remains necessary — the corroboration that exists favors the mutated-problem reading.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.66: the rule's coordination yield (abolition of the universal war pretext) is real, but its cost incidence is sharply asymmetric — protection is assigned to the very states that prey on their subjects, and the populations priced out of external recourse number in the hundreds of millions. Suppression is authored at 0.66 as a raw structural property, unscaled by power or scope: the rule's persistence depends on active defense — veto discipline over enforcement authorization, bloc voting against conditionality language, treatment of unauthorized rescue as aggression — while rival formulas survive in text but not in operation. Theater_ratio 0.52 reflects the contemporary share of norm-invocation that is selective or hypocritical (invoked against rivals' breaches, suspended for one's own); the function is not yet mostly performance. Accessibility_collapse 0.50: understanding the rule does not exhaust alternatives — humanitarian intervention, responsibility-to-protect, and capacity-graded formulas remain doctrinally alive and occasionally practiced. Resistance 0.62: advocacy networks, reforming states, and breaching great powers contest it continuously. The temporal series runs on one shared eight-point grid (every tracked metric authored at every point) and shows a cycle, not a trend line: Cold War hardening (1960-1975), post-Cold-War relaxation (1990), Kosovo-era strain (1999), the 2005 responsibility-to-protect concession, and the post-Libya ratchet (2011-2025). The oscillation tracks crisis-reform-relaxation-accumulation phases; base_properties values are measured at the 2025 endpoint, the ratchet-high phase. Claim and metrics are independent authored facts: claimed_type records my structural read (a genuine coordination function joined to asymmetric extraction under active enforcement); the metric values record the arrangement's observed operation; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the authoritarian-elite seat the arrangement is legitimate order itself — the alternative is universal crusade. From the trapped-population seat the same arrangement is abandonment codified: protection assigned to the predator. From the P5 seat it is a discretionary instrument — a gate they control, honored when convenient. Liberal democracies straddle: they codified the rule, chafe under it, and breach it episodically when domestic pressure forces the question. These divergences are computed by the engine from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. authoritarian_regime_elites sit nearest the beneficiary pole: the rule subsidizes them with impunity, and their exit is identity-locked (regime legitimacy is fused with the no-outsider-judgment principle), so effective extraction inverts toward subsidy. security_council_permanent_members derive low d as agenda-setters who collect the same shield they administer, tempered by exposure to counter-coalitions when they breach. nonaligned_diplomatic_bloc derives low-to-moderate d: genuine insurance benefit, paid in foreclosed solidarity options. liberal_democratic_governments are dual-declared (payer primary, beneficiary secondary): they pay in foreclosed humanitarian options and ad hoc breach costs while collecting reciprocal inviolability and systemic stability — the derivation should place them mid-range, nearer symmetric than either pure seat. populations_under_authoritarian_rule and persecuted_domestic_minorities sit at the full-target pole: powerless, trapped, bearing the entire cost side with zero recourse. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the arrangement as pure extraction ignores the real collective good — the abolition of universally licensed war, which held through eras when every state had doctrinal grounds to invade every other. Reading it as pure coordination ignores the shield's asymmetry: the same rule that restrains rescuers immunizes predators, and the parties who pay are constitutionally absent from the forums that maintain the rule. The founding problem as originally constituted (interstate war over internal confession) is dead, but the arrangement did not atrophy into performance: it acquired a mutated function (generalized restraint on pretext warfare) that remains load-bearing — which is why the theater ratio sits near half rather than dominance and why the type is hybrid rather than vestigial. founding_problem_status is authored 'contested' because the parties genuinely dispute whether the mutated problem is live; the mismatch consumer should read that dispute against the world_rearranges verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is categorical territorial inviolability the correct instantiation of the westphalia_sovereignty kernel, or do the conditional_responsibility and graded_sovereignty readings capture the kernel''s operative content?',
    'Track doctrinal trajectory in ICJ jurisprudence, General Assembly voting, and state practice: if conduct-conditional exceptions become routine lawful practice, the categorical reading loses descriptive grip and the sibling files'' classifications become the operative ones.',
    'Under conditional_responsibility the victim set expands to protected populations and the intervention barrier drops; under graded_sovereignty extraction concentrates on low-capacity states. This file''s epsilon and classification hold only while the categorical reading remains the operative norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the sovereignty kernel is operative').

omega_variable(
    anarchic_necessity_vs_construction,
    'Is the non-intervention rule a structural feature of the anarchic state system that would regenerate in any multipolar order, or a constructed arrangement maintained by identifiable defenders?',
    'Comparative systems analysis: periods of hegemonic shift and institutional breakdown where the rule lapsed despite unchanged anarchy (interwar era, post-1990 unilateralism) indicate construction; persistence through those windows indicates structural necessity.',
    'If structurally necessary, the arrangement trends toward the fixed/natural profile and its costs are a constant of the system; if constructed, its persistence depends on the defender coalition and it classifies alongside other actively enforced human arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anarchic_necessity_vs_construction, conceptual, 'Natural-law versus constructed status of the non-intervention rule').

omega_variable(
    victim_set_boundary,
    'Who bears the arrangement''s costs — only populations under authoritarian control, or also populations under decent government whose potential rescuers are legally foreclosed?',
    'Counterfactual welfare comparison of intervention and non-intervention episodes matched on atrocity type and scale, separating ruler-shield effects from rescuer-foreclosure effects.',
    'A wider victim set raises measured extraction and strengthens the asymmetric-cost component; a narrow set confines the cost accounting to authoritarian jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Boundary of the population bearing the rule''s costs').

omega_variable(
    selective_enforcement_asymmetry,
    'Does the rule bind weak states categorically while powerful states treat it as optional, so that measured costs concentrate on the weak twice over?',
    'Event-history dataset crossing intervention occurrences with invocations of the rule by power tier; test whether breach frequency and subsequent immunity correlate with state capability.',
    'Confirmed asymmetry raises effective extraction for weak-state targets and pushes the arrangement toward the pure-extraction profile for those seats; refutation supports the reciprocal-forbearance account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_asymmetry, empirical, 'Whether enforcement selectivity doubles the burden on weak states').

omega_variable(
    ratchet_or_cycle_post_2025,
    'Is the post-2011 hardening (veto discipline, sovereigntist revival) a permanent ratchet toward pure extraction, or another phase of the recurring relaxation-hardening cycle?',
    'Continue the temporal series past 2025: a ratchet shows monotone suppression_requirement growth across a full crisis cycle; a cycle shows relaxation once the current crisis cluster resolves.',
    'Ratchet confirmation dates a transition toward the pure-extraction profile; cycle confirmation keeps the hybrid classification with oscillating metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_or_cycle_post_2025, empirical, 'Whether current hardening is ratchet or cycle phase').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_ani_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.18).
narrative_ontology:measurement_basis(westphalia_ani_tr_t1945, observed).
narrative_ontology:measurement(westphalia_ani_tr_t1960, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(westphalia_ani_tr_t1960, observed).
narrative_ontology:measurement(westphalia_ani_tr_t1975, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1975, 0.36).
narrative_ontology:measurement_basis(westphalia_ani_tr_t1975, observed).
narrative_ontology:measurement(westphalia_ani_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(westphalia_ani_tr_t1990, observed).
narrative_ontology:measurement(westphalia_ani_tr_t1999, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1999, 0.33).
narrative_ontology:measurement_basis(westphalia_ani_tr_t1999, observed).
narrative_ontology:measurement(westphalia_ani_tr_t2005, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(westphalia_ani_tr_t2005, observed).
narrative_ontology:measurement(westphalia_ani_tr_t2011, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2011, 0.47).
narrative_ontology:measurement_basis(westphalia_ani_tr_t2011, observed).
narrative_ontology:measurement(westphalia_ani_tr_t2025, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(westphalia_ani_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(westphalia_ani_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement_basis(westphalia_ani_be_t1945, observed).
narrative_ontology:measurement(westphalia_ani_be_t1960, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement_basis(westphalia_ani_be_t1960, observed).
narrative_ontology:measurement(westphalia_ani_be_t1975, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement_basis(westphalia_ani_be_t1975, observed).
narrative_ontology:measurement(westphalia_ani_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(westphalia_ani_be_t1990, observed).
narrative_ontology:measurement(westphalia_ani_be_t1999, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1999, 0.59).
narrative_ontology:measurement_basis(westphalia_ani_be_t1999, observed).
narrative_ontology:measurement(westphalia_ani_be_t2005, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement_basis(westphalia_ani_be_t2005, observed).
narrative_ontology:measurement(westphalia_ani_be_t2011, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement_basis(westphalia_ani_be_t2011, observed).
narrative_ontology:measurement(westphalia_ani_be_t2025, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement_basis(westphalia_ani_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_ani_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.44).
narrative_ontology:measurement_basis(westphalia_ani_su_t1945, observed).
narrative_ontology:measurement(westphalia_ani_su_t1960, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1960, 0.56).
narrative_ontology:measurement_basis(westphalia_ani_su_t1960, observed).
narrative_ontology:measurement(westphalia_ani_su_t1975, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1975, 0.61).
narrative_ontology:measurement_basis(westphalia_ani_su_t1975, observed).
narrative_ontology:measurement(westphalia_ani_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement_basis(westphalia_ani_su_t1990, observed).
narrative_ontology:measurement(westphalia_ani_su_t1999, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1999, 0.49).
narrative_ontology:measurement_basis(westphalia_ani_su_t1999, observed).
narrative_ontology:measurement(westphalia_ani_su_t2005, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement_basis(westphalia_ani_su_t2005, observed).
narrative_ontology:measurement(westphalia_ani_su_t2011, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement_basis(westphalia_ani_su_t2011, observed).
narrative_ontology:measurement(westphalia_ani_su_t2025, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(westphalia_ani_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Westphalian sovereignty' per the epsilon-invariance principle: categorical inviolability (this file, epsilon 0.66 — real coordination function, asymmetric shield, actively enforced), conditional responsibility (separate file — forfeiture upon failure to protect; expanded protected set, lowered barrier), and graded sovereignty (separate file — capacity-scalar legitimacy; costs concentrate on low-capacity states). The categorical reading forecloses both siblings within a single framework while coexisting with them across the system's factions; edges declared to both. Upstream/downstream is unresolved among the three — each cites the others as the degenerate or utopian pole — so the family is linked symmetrically pending doctrinal-trajectory evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
