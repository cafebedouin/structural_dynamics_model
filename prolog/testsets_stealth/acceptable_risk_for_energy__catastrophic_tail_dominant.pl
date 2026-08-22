% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Dominant Risk Acceptability Regime for Energy Systems
 *   domain: risk assessment/energy policy/public safety governance
 *
 * SUMMARY:
 *   Since the 1975 Rasmussen report introduced probability-weighted
 *   core-damage analysis, the governing arrangement for energy risk
 *   acceptability has been progressively rebuilt around tail dominance:
 *   licensing thresholds keyed to worst-case consequences, irreversibility
 *   treated as veto-grade, waste disposal elevated from an engineering
 *   parameter to a binding constraint, and probabilistic trade-off framing
 *   denied standing in public deliberation. The arrangement delivers genuine
 *   protection to host and downwind communities and to future generations,
 *   sustains a large advocacy and regulatory apparatus, and — through slowed
 *   nuclear deployment — quietly preserves market share for fossil incumbents
 *   whose own casualty profile never receives equivalent scrutiny. Its costs
 *   fall on nuclear operators and vendors, on ratepayers, and on fossil host
 *   communities whose chronic mortality registers weakly under tail-dominant
 *   criteria. The ε referent is the standing regulatory arrangement as THIS
 *   reading assesses it — including its undischarged waste-isolation promise,
 *   which by the reading's own lights is a failure to deliver the protection
 *   the arrangement charges for — not the arrangement any sibling reading
 *   would build. The claim/metric gap is deliberate: claimed_type is authored
 *   from structural belief (genuine coordination function plus asymmetric
 *   extraction plus active enforcement), metrics from descriptive belief; the
 *   engine computes per-seat types independently. Sibling readings are
 *   separate files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - nuclear_regulators: agenda setter (institutional/identity_locked) — writes thresholds, fee-funded by licensees, blame-insulated by absolute standards
 *   - nuclear_operators_and_vendors: primary target (institutional/constrained) — bears worst-case-scaled compliance costs on sunk capital
 *   - electricity_ratepayers: target (moderate/constrained) — finances the cost structure through tariffs
 *   - downwind_and_neighbor_communities: protected beneficiary (moderate/constrained) — receives the absolute-threshold promise
 *   - future_generations: protected beneficiary (powerless/trapped) — present only through proxy institutions
 *   - anti_nuclear_advocacy_movements: beneficiary (organized/identity_locked) — collects standing, membership, and purpose
 *   - fossil_generation_incumbents: incidental beneficiary (institutional/arbitrage) — inherits deployment share as nuclear slows
 *   - fossil_host_communities: unregistered bearer of chronic risk (powerless/trapped)
 *   - probabilistic_risk_analysts: excluded voice (moderate/mobile) — framing denied standing in the legitimacy structure
 *   - independent_safety_assessors: analytical observer (analytical/analytical) — audits both sides, enforces nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.7).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.76).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.7).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Dominant Risk Acceptability Regime for Energy Systems").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk assessment/energy policy/public safety governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'bca89a47-dd19-4a9c-98be-dda0adb744e7').
narrative_ontology:cs_kernel_codification('bca89a47-dd19-4a9c-98be-dda0adb744e7', formalized).
narrative_ontology:cs_authority_grounding('bca89a47-dd19-4a9c-98be-dda0adb744e7', lineage).
narrative_ontology:cs_interpretation_layer_present('bca89a47-dd19-4a9c-98be-dda0adb744e7').
narrative_ontology:cs_reading_relation('bca89a47-dd19-4a9c-98be-dda0adb744e7', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('bca89a47-dd19-4a9c-98be-dda0adb744e7', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('bca89a47-dd19-4a9c-98be-dda0adb744e7', foundational, irreversibility_veto_over_expected_value).
narrative_ontology:cs_axiom_status(irreversibility_veto_over_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('bca89a47-dd19-4a9c-98be-dda0adb744e7', irreversibility_veto_over_expected_value, deontological).
narrative_ontology:cs_axiom('bca89a47-dd19-4a9c-98be-dda0adb744e7', secondary, tail_focus_optimal_under_deep_uncertainty).
narrative_ontology:cs_axiom_status(tail_focus_optimal_under_deep_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('bca89a47-dd19-4a9c-98be-dda0adb744e7', tail_focus_optimal_under_deep_uncertainty, instrumental).
narrative_ontology:cs_reference_frame('bca89a47-dd19-4a9c-98be-dda0adb744e7', absolute_tail_protection_baseline).
narrative_ontology:cs_drift_state('bca89a47-dd19-4a9c-98be-dda0adb744e7', contemporary_risk_informed_recalibration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bca89a47-dd19-4a9c-98be-dda0adb744e7', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, downwind_and_neighbor_communities).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_movements).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_generation_incumbents).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_vendors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, strong_precautionary_principle).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_weighting_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_fiduciary_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the licensing thresholds that decide which plants may operate and what waste pathways count as solved. Agencies are funded substantially through fees paid by the licensees they oversee, so review depth and staffing scale with the stringency they themselves set. Careers are staked on no catastrophic release occurring on their watch, and professional identity has fused with the guardianship of that record; leaving means exiting the safety establishment altogether.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, identity_locked, national).

% Organizes intervention in licensing proceedings, supplies the public vocabulary of irreversibility and inherited burden, and litigates to hold thresholds in place. Membership, funding, and institutional standing flow from the continuing struggle; the movement's purpose is constituted by opposition to nuclear deployment, and demobilization would dissolve much of what holds it together.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_movements, beneficiary,
    organized, generational, identity_locked, global).

% Lives near reactors, fuel-cycle sites, and waste transport routes. Receives emergency-planning infrastructure, monitoring, and an absolute-threshold promise that no permitted activity may impose catastrophic exposure on them. Cannot individually renegotiate the terms of neighboring facilities; relocating is possible but costly in property value and community ties.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, downwind_and_neighbor_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Will inherit either isolated waste or indefinitely stored spent fuel, and either the deployed or the foregone energy system. Cannot consent, bargain, or exit now; institutions act as proxies claiming to speak in their name. Their stake spans centuries and outlasts every currently living party.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Designs, builds, and operates plants whose compliance costs scale with worst-case scenarios rather than probability-weighted risk. Capital is sunk into licensed designs and sites; abandoning the business line strands those assets, so exit means writing off the franchise. Operates globally under national licensing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_vendors, payer,
    institutional, biographical, constrained, global).

% Pays tariffs that finance compliance margins, extended review timelines, and occasionally abandoned projects. Receives grid service and, in some systems, the low-carbon output the fleet provides. Cannot opt out of the financing arrangements embedded in rates; household budgets feel the costs immediately while the protective benefits are diffuse and deferred.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers, beneficiary).

% Sells generation from sources whose routine mortality and occasional large accidents draw none of the scrutiny the tail-dominant apparatus applies to nuclear. As nuclear deployment slows, their market share and asset lives extend. Can shift portfolios across fuels and jurisdictions at will, arbitraging whatever regime each jurisdiction adopts.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_generation_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Hosts plants whose particulate burden and accident history impose steady mortality that tail-dominant criteria barely register. Lacks the political leverage to reorder which risks receive the absolute-threshold treatment, and income and housing tie residents to place. Bears real costs while occupying no recognized seat in the risk conversation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_host_communities, payer,
    powerless, biographical, trapped, local).

% Produces the probability-weighted figures that quantify how small the catastrophic tails actually are. Inside licensing forums their framing loses standing whenever it favors deployment — the venue treats trading probabilities against consequences as inadmissible. Publishes in technical journals with no lever on the legitimacy structure; skills transfer readily to finance and other industries, and many have left.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, excluded,
    moderate, biographical, mobile, national).

% Audits both the regime's protective claims and its critics' cost claims; commissions comparative mortality and consequence studies across energy sources. Holds no enforcement power and no stake in either deployment or prohibition, which is what makes the seat usable as a check on the others.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, independent_safety_assessors, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_generation_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a collective commitment device for authorizing activities whose worst cases exceed any single actor's ability to compensate or clean up: no deployer, investor, or host community can privately trade away third parties' or descendants' exposure to catastrophic tails, so the arrangement fixes absolute thresholds and vests veto power in a common authority. It also solves a trust problem — a license signals that someone unaccountable to the deployer has judged the tail acceptable.
% TRANSFER_FUNCTION: Moves compliance costs and deployment delay from nuclear operators and vendors (financed by ratepayers) toward diffuse protection for host and downwind communities and future generations; moves decision authority over others' tail exposure to regulators and courts; and, through slowed nuclear deployment, preserves generation share and asset life for incumbent fossil suppliers.
% ABSENT_VOICES: Probabilistic risk analysts are present in technical annexes but stripped of standing when their conclusions favor deployment — the forum treats their framing as morally inadmissible, so the excluded seat is occupied by people physically in the room and procedurally silenced. Fossil host communities are absent in the stronger sense: their chronic, probabilistic mortality does not register under tail-dominant criteria, so no seat speaks for reordering the risk hierarchy. Future generations are present only through proxy institutions that claim their voice without their consent.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen licensing criteria to probability-weighted thresholds, unfreeze stalled construction and waste-siting processes, reprice nuclear assets, strand advocacy institutions built around opposition, and redirect generation investment toward whichever source the reopened calculus favors — the energy system's risk constitution would rebuild itself within a decade, and every named seat would find its position altered.
% FOUNDING_PROBLEM: After the hydrogen-bomb tests, early reactor accidents, and Chernobyl demonstrated that civilian energy technology can produce consequences exceeding any available compensation, cleanup capacity, or institutional memory, democracies faced the problem of how to authorize such activities at all: no willing buyer can purchase consent from people who do not yet exist or who were never asked.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: National Academies and IAEA waste-isolation assessments attest that the intergenerational hazard is real and unresolved; reinsurers' actuarial studies attest that offsite catastrophic losses can exceed liability caps by orders of magnitude; the historical record of Chernobyl and Fukushima attests the tail is not hypothetical. The same external sources also document that the arrangement's costs exceed risk-proportional allocation — corroboration of the founding problem is not corroboration of the current cost structure, and the reading's own assessment prices that gap into epsilon.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because compliance costs scale with worst-case scenarios rather than risk contribution, while the arrangement's signature protective deliverable — permanent waste isolation — remains undischarged after five decades, so much of what is paid purchases stalemate and documentation rather than protection. Suppression is high (0.76) and is authored as a raw structural property: the arrangement actively strips probabilistic trade-off framing of standing in licensing forums; per the framework, suppression is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater ratio (0.48) reflects recurring waste-'solution' processes that reopen roughly every decade without resolving, plus compliance documentation whose volume grows faster than any measurable risk reduction; it peaked after the 2011 Fukushima event (t=36) as stress-test and phase-out activity mixed genuine reassessment with reassurance performance, then eased modestly as some jurisdictions quietly readopted risk-informed methods. Accessibility collapse is moderate-low (0.40): expected-value and comparative framings remain intellectually available and industrially practiced — the arrangement suppresses their standing rather than erasing them. Resistance (0.65) is sustained industry, economic, and post-climate-crisis environmental pushback. All three metric series run on ONE shared time grid (t = 0, 10, 20, 30, 36, 44, 51, spanning 1975–2026) so every metric is authored at every examined point; the suppression_requirement series is authored because the story specifically tracks enforcement-machinery evolution (admissibility rules, hearing structures, post-accident ratchets), not merely static suppression.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the agenda-setter seat the arrangement is a hard-won civilizational lesson institutionalized: Chernobyl and Fukushima are remembered as proof that no probability is small enough to authorize an unrecoverable outcome, and the regulator's own blame-insulation feels like diligence rather than privilege. From the payer seats the same structure operates as costs decoupled from risk contribution — an operator pays as if catastrophe were certain, a ratepayer finances reviews that outlast the plant's construction. Protected beneficiaries experience the arrangement as the only thing standing between them and exposures they never contracted to. Fossil incumbents experience it as a quiet subsidy they did not ask for and do not administer. Fossil host communities experience the inverse: their chronic, probabilistic mortality is structurally invisible to a calculus tuned exclusively to tails. Excluded analysts experience the arrangement as the delegitimation of their craft. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (downwind_and_neighbor_communities, future_generations, anti_nuclear_advocacy_movements, fossil_generation_incumbents) derive low directionality — the arrangement subsidizes them; declared victims (nuclear_operators_and_vendors, electricity_ratepayers, fossil_host_communities) derive high directionality — it extracts from them, amplified for the trapped (future_generations, fossil_host_communities) relative to the merely constrained. Two overrides are needed where the derivation chain cannot see the true relationship. First, nuclear_regulators (institutional, d=0.15): as administrators they appear in no beneficiary list, but the arrangement subsidizes them directly — licensee fees fund the agency, review depth scales with stringency they themselves set, and absolute standards insulate them from the calculated-residual-risk blame they would bear under probabilistic thresholds. Second, probabilistic_risk_analysts (moderate, d=0.85): they appear in no beneficiary or victim list, so they would fall to a canonical fallback, but the arrangement operates directly on their professional practice by denying its framing standing — they are targets of the suppression mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy is declared: catastrophic-capable activities still require authorization, waste still accumulates at dozens of interim sites, and new designs still confront the same consent problem. The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure extraction ignores the genuine coordination function — no private actor can purchase consent from the unborn or the unasked, and the absolute-threshold commitment device solves a real collective-action problem that expected-value contracting cannot. Reading it as pure coordination ignores the asymmetric extraction — costs scaled to worst cases rather than risk contribution, an undelivered core protective promise, and enforcement machinery whose principal observable output for decades has been the exclusion of a rival framing rather than reduced harm. The theater_ratio series is the early-warning instrument for the piton trajectory: if waste isolation is someday achieved and tails engineered down while the apparatus persists, the founding problem dies, the mismatch flag fires, and the honest successor frame is a transitional scaffold winding down — not the perpetual regime the current structure implies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is catastrophic-tail dominance a discovered requirement of rational risk acceptance under deep uncertainty, or a constructed priority whose application pattern serves identifiable constituencies?',
    'Test the regime''s consistency: if tail-weighting were applied uniformly across energy sources (large dam failures, gas incidents, coal ash impoundments) and survived contact with those cases, the weighting is principled; if it tracks the one technology whose opponents hold agenda-setting power, it is constructed.',
    'A constructed finding pushes the arrangement toward enforced extraction riding a partial coordination function; a principled finding strengthens the coordination reading and lowers effective extraction for the protected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether this reading of the acceptable-risk kernel reflects discovered rational necessity or constructed advantage.').

omega_variable(
    sibling_delta_expected_value,
    'What would the expected_value_dominant sibling reading change structurally if it governed the same arrangement?',
    'Counterfactual authorship of the sibling story: the victim set contracts to parties bearing uncompensated probability-weighted harm; suppression of trade-off framing falls to background levels; waste disposal converts from binding constraint to a costed engineering option among others.',
    'Effective extraction borne by the nuclear sector and ratepayers drops sharply under the sibling''s computation; the disagreement between readings is located precisely in the weighting function over probability and consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_expected_value, conceptual, 'Structural delta if the expected-value sibling reading governed instead.').

omega_variable(
    sibling_delta_comparative_risk,
    'What would the comparative_risk_dominant sibling reading change structurally?',
    'Counterfactual authorship of the sibling story: the victim set re-sorts by per-unit-harm comparison across sources — nuclear exits the victim set, fossil generation enters it — and no absolute threshold survives anywhere in the framework.',
    'The arrangement would read as misdirected rather than extractive; the disagreement is located in whether acceptability is absolute or exists only relative to competing energy risks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_comparative_risk, conceptual, 'Structural delta if the comparative-risk sibling reading governed instead.').

omega_variable(
    intergenerational_ledger_netting,
    'Does the regime''s protection of future generations against waste tails exceed, or fall short of, the burdens the same cohorts bear from indefinitely stored spent fuel and from the foregone deployment the regime''s cost structure causes?',
    'Integrated assessment netting realized isolation performance, projected interim-storage duration, and counterfactual generation-mix impacts on the identical future cohorts the regime claims to protect.',
    'If the net position is negative for those cohorts, the future_generations beneficiary declaration partially inverts and effective extraction shifts further upward for the arrangement as a whole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_ledger_netting, empirical, 'Whether the intergenerational protection ledger nets positive or negative for the cohorts involved.').

omega_variable(
    discursive_suppression_mechanism,
    'Is the suppression of probabilistic trade-off framing carried by structural rules (hearing admissibility standards, licensing criteria) or by internalized norms (analyst self-censorship, public treatment of trade-off talk as taboo)?',
    'Compare the survival of probability-weighted arguments inside technical venues versus public forums, and track the post-career speech of former regulators and agency staff once institutional penalties lapse.',
    'If the internalized component is substantial, suppression persists even after admissibility rules liberalize, and the scalar suppression measure understates the durable force the arrangement would retain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_suppression_mechanism, empirical, 'Structural versus internalized mechanism behind the suppression of probabilistic framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 51).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.46).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t36, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 36, 0.52).
narrative_ontology:measurement_basis(acce_tr_t36, observed).
narrative_ontology:measurement(acce_tr_t44, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 44, 0.5).
narrative_ontology:measurement_basis(acce_tr_t44, observed).
narrative_ontology:measurement(acce_tr_t51, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 51, 0.48).
narrative_ontology:measurement_basis(acce_tr_t51, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t36, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 36, 0.73).
narrative_ontology:measurement_basis(acce_be_t36, observed).
narrative_ontology:measurement(acce_be_t44, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 44, 0.72).
narrative_ontology:measurement_basis(acce_be_t44, observed).
narrative_ontology:measurement(acce_be_t51, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 51, 0.7).
narrative_ontology:measurement_basis(acce_be_t51, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t36, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 36, 0.8).
narrative_ontology:measurement_basis(acce_su_t36, observed).
narrative_ontology:measurement(acce_su_t44, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 44, 0.77).
narrative_ontology:measurement_basis(acce_su_t44, observed).
narrative_ontology:measurement(acce_su_t51, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 51, 0.76).
narrative_ontology:measurement_basis(acce_su_t51, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_mandate).

% DUAL FORMULATION NOTE:
% The colloquial label 'acceptable risk for energy' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family: catastrophic_tail_dominant (this file), expected_value_dominant, and comparative_risk_dominant. Each authors its own epsilon over the SAME standing arrangement: this reading finds high extraction because the arrangement fails its own protective promise (undisposed waste) while imposing worst-case-scaled costs; the expected-value reading would find the arrangement only mildly extractive; the comparative reading would find it misdirected rather than extractive. This upstream reading structurally influences both siblings — its thresholds define the baseline the others argue against — and it generates the downstream nuclear_waste_disposal_mandate story, in which waste disposal hardens from engineering parameter into binding constraint. All family members link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
