% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Substantial Effects Doctrine with Jurisdictional Nexus and Non-Pretextual Economic Regulation Limits
 *   domain: constitutional_law/federalism/commerce_regulation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the commerce_clause_text kernel:
 *   the substantial-effects-limited reading, under which federal power
 *   reaches intrastate activity with substantial effects on interstate
 *   commerce but only where the statute carries a jurisdictional element and
 *   regulates genuinely economic rather than pretextually relabeled
 *   police-power matters. The ε referent is the standing arrangement under
 *   contest — the doctrine as operative constitutional law from Lopez (1995)
 *   through the present — assessed by this reading's own lights, never the
 *   arrangements the sibling readings would install. The structural signature
 *   is a hybrid beneficiary architecture: the enabling half
 *   (aggregate-effects reach) benefits Congress and federal agencies and
 *   burdens trapped economic actors; the limiting half (nexus plus
 *   economic-character tests) benefits state governments and non-economic
 *   participants. Category-boundary policing — drawing and patrolling the
 *   economic/non-economic line — is the mechanism through which both halves
 *   operate. Per the ε-invariance principle, the sibling readings
 *   (expansive_federal_reading, originalist_narrow_reading) are separate
 *   stories with their own ε, beneficiary sets, and classifications; this
 *   file links them via network.affects_constraints and does not average
 *   across them.
 *
 * KEY AGENTS:
 *   - - supreme_court: Agenda-setter (institutional/analytical) — draws and polices the economic/non-economic boundary; its precedents are the constraint's operating code
 *   - - congress: Agenda-setter and beneficiary (institutional/mobile) — legislates within the tests, receives the jurisdiction the enabling half grants
 *   - - federal_enforcement_agencies: Beneficiary (institutional/constrained) — administer statutes sustained by the doctrine
 *   - - state_governments: Beneficiary with mixed exposure (institutional/constrained) — hold the reserved police-power domain, lose ground to preemption on the economic side
 *   - - noneconomic_activity_participants: Beneficiary (moderate/constrained) — immune from federal reach so long as courts characterize their conduct as non-economic
 *   - - regulated_intrastate_economic_actors: Primary target (moderate/trapped) — bear quotas, standards, and reporting duties without consent
 *   - - home_production_households: Primary target (powerless/trapped) — swept in by aggregation of subsistence-level production
 *   - - preemption_affected_localities: Excluded voice (moderate/trapped) — displaced by federal occupation of regulatory fields, no seat in the doctrine's construction
 *   - - commerce_clause_scholars: Analytical observer — maps the full structure from outside the collection and payment flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.58).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.55).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Substantial Effects Doctrine with Jurisdictional Nexus and Non-Pretextual Economic Regulation Limits").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism/commerce_regulation").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, '7760a079-5a6e-495d-9341-75156656ee2d').
narrative_ontology:cs_kernel_codification('7760a079-5a6e-495d-9341-75156656ee2d', fixed_text).
narrative_ontology:cs_authority_grounding('7760a079-5a6e-495d-9341-75156656ee2d', lineage).
narrative_ontology:cs_interpretation_layer_present('7760a079-5a6e-495d-9341-75156656ee2d').
narrative_ontology:cs_reading_relation('7760a079-5a6e-495d-9341-75156656ee2d', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('7760a079-5a6e-495d-9341-75156656ee2d', commerce_clause_text__originalist_narrow_reading, forecloses).
narrative_ontology:cs_axiom('7760a079-5a6e-495d-9341-75156656ee2d', foundational, substantial_effects_reach_is_conditional).
narrative_ontology:cs_axiom_status(substantial_effects_reach_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('7760a079-5a6e-495d-9341-75156656ee2d', substantial_effects_reach_is_conditional, conventional).
narrative_ontology:cs_axiom('7760a079-5a6e-495d-9341-75156656ee2d', foundational, economic_character_determines_federal_reach).
narrative_ontology:cs_axiom_status(economic_character_determines_federal_reach, holdable).
narrative_ontology:cs_axiom_grounding('7760a079-5a6e-495d-9341-75156656ee2d', economic_character_determines_federal_reach, empirically_contingent).
narrative_ontology:cs_axiom('7760a079-5a6e-495d-9341-75156656ee2d', secondary, jurisdictional_elements_required_in_statutes).
narrative_ontology:cs_axiom_status(jurisdictional_elements_required_in_statutes, holdable).
narrative_ontology:cs_axiom_grounding('7760a079-5a6e-495d-9341-75156656ee2d', jurisdictional_elements_required_in_statutes, conventional).
narrative_ontology:cs_reference_frame('7760a079-5a6e-495d-9341-75156656ee2d', bounded_substantial_effects_framework).
narrative_ontology:cs_drift_state('7760a079-5a6e-495d-9341-75156656ee2d', contemporary_post_raich_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('7760a079-5a6e-495d-9341-75156656ee2d', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, congress).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_enforcement_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, noneconomic_activity_participants).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, regulated_intrastate_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, home_production_households).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, aggregate_effects_theory).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, police_power_reservation).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, anti_pretext_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which federal statutes reach intrastate conduct: applies the substantial-effects test, checks for a jurisdictional element tying the regulated activity to interstate commerce, and examines whether the statute governs genuinely economic activity or dresses a local police matter in commercial language. Its opinions define where the line sits; it can strike statutes that fail the tests and has done so rarely since 2000.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Writes market-regulating statutes within the doctrine's terms: includes jurisdictional elements, compiles legislative findings on aggregate effects, and frames regulations in economic language. Gains a durable, court-legitimated instrument for governing national markets; pays the cost of drafting inside the tests and occasionally loses statutes to them.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, congress, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__substantial_effects_limited_reading, congress, beneficiary).

% Administer and enforce statutes sustained by the doctrine — agricultural quotas, wage and hour rules, environmental controls, controlled-substance regimes. Their mandates depend on the courts continuing to accept the statutes' commerce footing; they compile findings and defend the statutes in litigation.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Retain primary authority over crime, family law, education, and land use so long as Congress acts through commerce rather than a general police power. They lose regulatory room where federal economic regulation preempts state schemes, and they periodically press the boundary through litigation and interstate compacts.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, state_governments, beneficiary,
    institutional, generational, constrained, regional).

% Individuals whose conduct — simple possession, personal cultivation without sale, locally defined harms — falls on the non-economic side of the line and therefore stays beyond federal commerce regulation. Their immunity depends on how courts characterize their activity; a shift in characterization places them inside federal reach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, noneconomic_activity_participants, beneficiary,
    moderate, biographical, constrained, national).

% Farmers, manufacturers, and service businesses whose intrastate production aggregates into national markets. They operate under federal quotas, price controls, labor standards, and reporting duties regardless of consent, and cannot leave the doctrine's reach while remaining in the national economy.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, regulated_intrastate_economic_actors, payer,
    moderate, biographical, trapped, national).

% Households growing grain, raising livestock, or cultivating crops for their own use whose output, aggregated with similar households nationwide, is treated as affecting national markets. They bear federal penalties and compliance duties for activity undertaken for subsistence or personal consumption.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, home_production_households, payer,
    powerless, immediate, trapped, local).

% Cities, counties, and school districts whose local ordinances are displaced when federal commerce-based regulation occupies a field. They would argue for subsidiarity — local control over local matters — but hold no institutional seat in the doctrine's construction; they appear only as litigants after preemption lands.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, preemption_affected_localities, excluded,
    moderate, biographical, trapped, local).

% Academic lawyers and historians who map the doctrine's development, test its categories against economic data, and publish criticism from every camp. They watch the full structure without collecting from it or bearing its burdens.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__substantial_effects_limited_reading, congress).
narrative_ontology:fixing_cost_class(commerce_clause_text__substantial_effects_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory authority between the federal government and the states for intrastate activities whose aggregate effects cross state lines, supplying a repeatable test — substantial effects plus a jurisdictional element plus genuinely economic character — that both Congress and the courts can apply to any proposed statute.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction, and through it compliance costs, enforcement exposure, and criminal liability, from state governments and local actors to the federal government, for activity classified as economic with substantial interstate effects.
% ABSENT_VOICES: State governments rarely sit at the table when the boundary is drawn — the defining cases are almost always federal government versus an individual defendant, not federal versus state. Regulated individuals enter the process only as defendants after a statute already covers them. Local governments facing preemption have no seat at all; they would argue for subsidiarity but are heard only through occasional amicus filings.
% DISAPPEARANCE_RATIONALE: If the nexus and non-pretextual limits vanished overnight, the operative rule would collapse into the expansive reading — federal power would reach any activity with arguable aggregate effects, the non-economic immunity would evaporate, and the states' reserved domain would shrink to whatever Congress chose not to occupy. If the substantial-effects extension itself vanished, the reverse: federal market governance would contract to border-crossing trade and decades of statutes would lose their constitutional footing. Either way the current federal-state allocation rearranges around whichever rule replaces it.
% FOUNDING_PROBLEM: A nationally integrated economy — commodity prices, labor markets, product markets that ignore state lines — governed by a constitution that enumerates federal powers and reserves the general police power to the states. The founding problem was bridging the gap between a commerce power literally aimed at trade crossing state borders and the reality that intrastate production decisions aggregate into national market conditions.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians document national commodity-price integration and the aggregate-market character of farm and labor decisions independently of any party's litigation interest; legal scholars across every camp concede the integration fact even while disputing the doctrinal response. The beneficiaries attest liveness self-interestedly — Congress keeps invoking the doctrine and agencies keep defending it — so the outside corroboration that matters is the economic-history record and the continued, cross-ideological congressional reliance on aggregate-effects findings for market-regulating statutes.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58: the doctrine imposes real, non-consensual burdens — production quotas, labor standards, substance regimes, penalties on household-scale production — but its limits spare the non-economic half of conduct and preserve state channels, placing it well below an unrestricted-reach arrangement and above a border-crossing-only one. Suppression is 0.55: persistence requires active judicial policing (statutes are struck when they fail the tests), regulated actors have no exit short of leaving the national economy, but the arrangement deliberately preserves alternative governance channels for non-economic matters, so coercion is bounded rather than totalizing. Theater ratio is 0.32 and rising across the interval: jurisdictional findings have become drafting boilerplate, and the economic-character label is sometimes applied ritually, though the tests still filter at the margin. Accessibility collapse is 0.55: once the doctrine is understood, the alternative of state-exclusive governance collapses for economic activity with aggregate effects but survives intact for non-economic matters. Resistance is 0.60: commerce-clause challenge is a standing litigation practice, with two landmark successes (Lopez, Morrison) and continuous scholarly and political contestation. The measurement series run on one shared time grid (1995–2025, seven points) so every tracked metric is authored at every examined point; the suppression_requirement series traces a genuine enforcement-capacity arc — machinery built up sharply from 1995 to 2000, moderated after Raich's deference to congressional economic findings, then plateaued — which is why it is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute different arrangements from identical doctrine. From the trapped economic actor's position, the arrangement is uncompensated subjection: their intrastate choices are governed by a distant sovereign on a theory (aggregation) they cannot rebut and cannot exit. From Congress's position, the same structure is a hard-won, court-legitimated instrument without which national market governance would be unconstitutional. From the Court's position, it is the institution's own crafted equilibrium — the thing that keeps the commerce clause doing distinctive work instead of collapsing into either a blank check or a dead letter. State governments experience both faces at once: protected on crime and family law, crowded out on anything Congress can frame as a market. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and the federal enforcement agencies sit near the beneficiary end: the enabling half converts directly into jurisdiction and enforceable mandates, and the limiting half, by making the power credible and sustainable, subsidizes rather than taxes them. Noneconomic_activity_participants sit near the beneficiary end as well — the economic-character test is their shield. Regulated_intrastate_economic_actors sit near the target end, amplified by trapped exit: aggregation theory reaches them wherever they operate inside the national economy. Home_production_households sit nearest the full-target end — powerless, immobile, and reached precisely through the aggregation of their smallest decisions. State_governments are the genuinely mixed seat: the derivation from their beneficiary declaration yields a low d, but their true position is higher because preemption taxes them on the economic side even as the reserve protects them elsewhere. No directionality_override is authored: overrides key on power_atom, and the institutional atom is shared by Congress, the agencies, and the states, so any correction for the states would misapply to the federal seats. The mixed position is documented here and in the boundary-stability omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing a nationally integrated economy under enumerated powers — is live, corroborated from outside the beneficiary set by economic-history scholarship and by continuing cross-ideological congressional reliance on aggregate-effects findings. The enabling half is functional, not vestigial. The limiting half, however, shows early atrophy signals: no major statute has been struck on nexus or economic-character grounds since Morrison (2000), jurisdictional findings have become boilerplate, and the theater_ratio series rises monotonically across the interval. The limiting_half_viability omega tracks whether the limits still perform or have become ceremonial. The tangled_rope claim is what prevents misclassification in both directions: reading the whole arrangement as pure extraction would erase the genuine coordination function (a workable federal-state allocation for aggregate-effects problems that no party has a workable substitute for), while reading it as pure coordination would erase the asymmetric, enforcement-backed burden concentrated on trapped economic actors and subsistence households. The R5 mismatch consumer should find no zombie flag here — status=live combined with verdict=world_rearranges — but the limiting half is the component to watch for mandatrophy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the commerce_clause_text kernel — how would the beneficiary/victim structure and classification change under the sibling readings, expansive_federal_reading (no substantive limits) and originalist_narrow_reading (border-crossing trade only)?',
    'Generate and classify the sibling stories and compare victim sets, epsilon, and seat divergence across the family; the deltas are structural, not rhetorical.',
    'Under the expansive reading the limits vanish: regulated-actor extraction rises, the non-economic immunity disappears, and state_governments shift from beneficiary toward target. Under the narrow reading the reach vanishes: the federal seats lose their beneficiary position, regulated economic actors gain immunity, and the coordination function contracts to border-crossing trade. The hybrid beneficiary structure documented here exists only under the limited reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Kernel-indexical dependence of the beneficiary/victim structure on which reading of the commerce clause is instantiated.').

omega_variable(
    economic_noneconomic_boundary_stability,
    'Is the economic/non-economic line a stable feature of activity types, or a movable artifact of judicial discretion that shifts with Court composition?',
    'Track characterization outcomes across decades and stress the line with new activity types — digital platforms, data markets, gig work — where economic character is contested; compare outcomes before and after membership changes on the Court.',
    'If the line is discretionary, the constraint''s incidence follows judicial composition rather than economic structure: the same conduct is federally governed or immune depending on who draws the boundary, and the beneficiary set for noneconomic_activity_participants becomes unstable across generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary_stability, conceptual, 'Whether the category boundary the constraint polices is a natural kind or a judicial artifact.').

omega_variable(
    pretext_detection_reliability,
    'Can courts reliably distinguish genuine commerce regulation from police-power regulation dressed in commercial language — the non-pretextual test''s core operation?',
    'Compare the stated economic purposes against the operative effects of upheld versus struck statutes; examine whether the test discriminates or merely ratifies whatever framing Congress supplies.',
    'If pretext detection fails, the limiting half degenerates into formality — the arrangement converges operationally toward the expansive reading while retaining the limited reading''s vocabulary, and the measured extraction understates the effective reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_detection_reliability, empirical, 'Reliability of the judicial machinery that separates economic regulation from disguised police-power regulation.').

omega_variable(
    limiting_half_viability,
    'Does the limiting half — jurisdictional nexus plus non-pretextual economic character — still perform a live filtering function, or has it become ceremonial given the absence of major strikes since Morrison (2000)?',
    'Count statutes struck versus upheld under the tests since 2000; assess whether any realistically probable future statute would fail; survey drafting practice to see whether jurisdictional elements are written to satisfy the Court or merely to recite the formula.',
    'If ceremonial, the arrangement is drifting toward its expansive sibling in operation while keeping the limited reading''s name — a mandatrophy trajectory localized to the limiting half, with theater_ratio as the leading indicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limiting_half_viability, empirical, 'Viability of the constraint''s limiting machinery versus its ceremonial residue.').

omega_variable(
    court_institutional_self_benefit,
    'Does the boundary-policing function persist partly because it sustains the Court''s own institutional power over federalism questions, independent of any participant-serving coordination value?',
    'Compare the doctrine''s vitality and the intensity of its policing across periods of varying institutional stakes and external pressure on the Court; examine whether policing intensifies when the Court''s gatekeeping role is otherwise threatened.',
    'If institutional self-benefit is material, part of the arrangement''s maintenance is the agenda-setter feeding itself, which shifts the computed classification toward the extractive end and reframes the supreme_court seat from neutral arbiter to interested administrator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(court_institutional_self_benefit, empirical, 'Whether the agenda-setter''s policing of the boundary serves participants or its own institutional position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cselr_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(cselr_tr_t2000, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(cselr_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(cselr_tr_t2010, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(cselr_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(cselr_tr_t2020, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2020, 0.32).
narrative_ontology:measurement(cselr_tr_t2025, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(cselr_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(cselr_be_t2000, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(cselr_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement(cselr_be_t2010, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(cselr_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(cselr_be_t2020, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(cselr_be_t2025, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cselr_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(cselr_su_t2000, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(cselr_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(cselr_su_t2010, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(cselr_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(cselr_su_t2020, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(cselr_su_t2025, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the commerce clause' decomposes into three structurally distinct claims per the epsilon-invariance principle: an expansive reach claim (all economic activity with substantial aggregate effects, effectively unconditional), a narrow originalist claim (border-crossing trade and instrumentalities only), and this limited claim (substantial-effects reach conditioned on jurisdictional nexus and non-pretextual economic character). Each carries its own epsilon, beneficiary set, and failure modes; forcing them into one story would make epsilon observer-relative, which the chi formula forbids. Upstream/downstream structure: the expansive reading's Wickard-lineage reach claim is cited as settled evidence by defenders of this limited reading (this reading accepts the reach and adds the limits), while the narrow reading contests the reach itself — so this story links to both siblings, and the family should be analyzed as a boundary-partition kernel rather than a linear chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
