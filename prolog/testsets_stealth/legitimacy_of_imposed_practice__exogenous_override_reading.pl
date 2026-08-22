% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Decree-Sufficiency Doctrine of Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing regime governs by a doctrine it never states as doctrine:
 *   that a duly promulgated decree replaces whatever practice it names, and
 *   that obedience follows from legality rather than conviction. The
 *   canonical test is the revolutionary calendar, civil time re-founded by
 *   statute, decimal hours and renamed months pressed on the country by
 *   commissars and tribunals, while rural France answered with two calendars,
 *   one for the administration and one for the fields, until the regime
 *   itself quietly restored the old reckoning. The same logic recurs wherever
 *   dress, ritual, or inheritance practice is abolished by fiat and policed
 *   into partial compliance. This story instantiates the
 *   exogenous_override_reading of the imposed-practice kernel: epsilon is
 *   authored for the standing decree-and-enforcement arrangement as that
 *   reading holds it, a real standardization gain purchased by uncompensated,
 *   unconsulted adjustment costs, held up only by continuing coercion, and
 *   decaying into ceremony. Family note: the colloquial label 'legitimacy of
 *   imposed practice' decomposes into three structurally distinct claims with
 *   distinct epsilon values (see network.dual_formulation_note); this file
 *   authors only the pure-decree claim and links its siblings.
 *
 * KEY AGENTS:
 *   - decreeing_central_state: agenda-setting issuer of the mandates (institutional/arbitrage) — writes and enforces the decrees, collects compliance statistics and fines, bears almost none of the adjustment cost
 *   - urban_administrative_elites: primary beneficiary (powerful/mobile) — staff the new system, convert at negligible personal cost, collect careers and standing
 *   - enforcement_magistrates: administering seat (institutional/mobile) — operate tribunals and penalty schedules, careers ride on prosecution counts
 *   - rural_peasant_communities: primary target (powerless/trapped) — bear conversion costs without consultation, answer with durable dual practice
 *   - provincial_merchants_and_clerks: paying intermediary (moderate/constrained) — double-entry survival between mandate and market, partial offsetting gain
 *   - parish_clergy: custodians of the displaced liturgical year (organized/constrained) — outward conformity, retained interior practice
 *   - unconsulted_rural_women: excluded carriers of domestic practice (powerless/trapped) — object from outside the room, registered only as residual non-compliance
 *   - analytical_historians: analytical observer — reconstruct the compliance-versus-internalization gap from the archive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.52).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Decree-Sufficiency Doctrine of Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '20b02963-d54e-4c81-bd36-78cc6c12fc1e').
narrative_ontology:cs_kernel_codification('20b02963-d54e-4c81-bd36-78cc6c12fc1e', formalized).
narrative_ontology:cs_authority_grounding('20b02963-d54e-4c81-bd36-78cc6c12fc1e', self_enforcing).
narrative_ontology:cs_reading_relation('20b02963-d54e-4c81-bd36-78cc6c12fc1e', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('20b02963-d54e-4c81-bd36-78cc6c12fc1e', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, forecloses).
narrative_ontology:cs_axiom('20b02963-d54e-4c81-bd36-78cc6c12fc1e', foundational, legal_mandate_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(legal_mandate_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('20b02963-d54e-4c81-bd36-78cc6c12fc1e', legal_mandate_sufficient_for_displacement, conventional).
narrative_ontology:cs_axiom('20b02963-d54e-4c81-bd36-78cc6c12fc1e', foundational, internalization_unnecessary_for_compliance).
narrative_ontology:cs_axiom_status(internalization_unnecessary_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('20b02963-d54e-4c81-bd36-78cc6c12fc1e', internalization_unnecessary_for_compliance, empirically_contingent).
narrative_ontology:cs_axiom('20b02963-d54e-4c81-bd36-78cc6c12fc1e', secondary, compliance_counts_measure_success).
narrative_ontology:cs_axiom_status(compliance_counts_measure_success, holdable).
narrative_ontology:cs_axiom_grounding('20b02963-d54e-4c81-bd36-78cc6c12fc1e', compliance_counts_measure_success, instrumental).
narrative_ontology:cs_reference_frame('20b02963-d54e-4c81-bd36-78cc6c12fc1e', legal_command_sovereignty).
narrative_ontology:cs_drift_state('20b02963-d54e-4c81-bd36-78cc6c12fc1e', post_repeal_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20b02963-d54e-4c81-bd36-78cc6c12fc1e', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, decreeing_central_state).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_magistrates).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasant_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, provincial_merchants_and_clerks).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, parish_clergy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, unconsulted_rural_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, provincial_merchants_and_clerks).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, exogenous_override_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, command_theory_of_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the legal mandates that abolish the prior calendar and prescribe new public practice, builds the offices and penalty schedules that carry them out, and judges the program by counts of formal compliance. It can amend or repeal its own decrees at will and bears almost none of the daily adjustment cost, which falls on those whose working and ritual lives the mandates reorder.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, decreeing_central_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff the ministries, courts, and schools that run on the new timetable and wear the prescribed civic forms. Adoption costs them little because the mandated practices resemble habits they already hold, while appointments, promotions, and social standing flow through demonstrated fluency in the new order.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elites, beneficiary,
    powerful, biographical, mobile, national).

% Operate the commissions and tribunals that fine the use of the abolished calendar and penalize refusal of prescribed dress. Their reports quantify prosecutions and convictions, and their careers advance on those numbers, which gives them a standing interest in finding violations to process.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_magistrates, agenda_setter,
    institutional, biographical, mobile, regional).

% Work land tied to saints' days, fairs, rents, and harvest rhythms set by the displaced calendar. The decrees arrived without consultation; they answer with quiet dual practice, official dates on paper and old dates in the fields, absorbing confusion in markets, taxes, and leases rather than relocating or abandoning the parish networks that organize their year.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasant_communities, payer,
    powerless, generational, trapped, regional).

% Keep ledgers twice, once in each calendar, to stay lawful with the administration and intelligible to customers. Standardized civil dating eventually simplifies some contracting, but the conversion years bring errors, disputes, and penalty exposure, and leaving trade for another jurisdiction means abandoning established credit networks.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, provincial_merchants_and_clerks, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, provincial_merchants_and_clerks, beneficiary).

% Their liturgical year anchors the community's time, and the decrees criminalize parts of its public observance. Outward conformity protects parishes from prosecution while familiar practice continues at baptisms, burials, and feast days; open refusal invites charges, and flight abandons the congregations they serve.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, parish_clergy, payer,
    organized, generational, constrained, regional).

% Anchor household ritual, foodways, and dress to the displaced practice and transmit it across generations, yet no deliberative body that drafted the mandates included them. Their objection surfaces only indirectly, as the persistence of old practice in domestic space that enforcement records register as residual non-compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, unconsulted_rural_women, excluded,
    powerless, generational, trapped, local).

% Reconstruct the gap between official compliance statistics and lived practice from notarial archives, parish registers, prefect reports, and private correspondence, and compare episodes across regimes to establish what legal mandates alone can and cannot move.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, analytical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, decreeing_central_state).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single state-prescribed timetable and public-practice regime lets taxation, contracts, military logistics, schooling, and official communication run on one schedule, and prescribed civic dress gives officials and citizens mutually legible markers of membership in the new polity.
% TRANSFER_FUNCTION: Moves the costs of conversion, meaning relearned dates, rewritten contracts, forfeited festivals, fines, and prosecution exposure, from the state onto unconsulted rural households, clergy, and provincial traders, while moving appointments, careers, and symbolic capital to urban administrators and enforcement cadres.
% ABSENT_VOICES: Rural communes, parish clergy, and the women who carried domestic practice were never seated in the conventions and councils that drafted the mandates; their dissent reaches the record only as non-compliance statistics, prosecution files, and prefect complaints about stubborn provinces.
% DISAPPEARANCE_RATIONALE: If the decree regime vanished overnight, dating on contracts, tax schedules, school calendars, and market days would revert to the suppressed practice within a season, enforcement careers and festival budgets would dissolve, and the state would lose the claim of instantaneous transformation its legitimacy narrative relied on; the administrative world built on the mandated timetable would visibly reorganize.
% FOUNDING_PROBLEM: A new regime facing a population whose loyalties were organized around the old church calendar, royal festivals, and inherited dress sought immediate, visible erasure of those markers; decree promised to replace centuries of accumulated practice with a rational civic order in a single legislative act.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the repealer's own 1806 message conceding the calendar served no useful purpose, prefect reports cataloguing rural non-compliance, notarial archives showing persistent dual dating, and prosecution records whose volume attests that mandate alone never secured the practice. The adverse record itself is the corroboration; no beneficiary-side attestation is relied upon.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness traces the episode arc: 0.64 at decree (immediate uncompensated conversion costs), peaking 0.74 when tribunals, fines, and mandatory dual bookkeeping press hardest, easing to 0.62 as enforcement yields and workarounds normalize; the end-state value anchors base_properties. Theater crosses 0.5 in the back third: revolutionary festivals, ceremonial decimal clocks, and compliance pageants increasingly substitute for the displacement they celebrate, the classic Goodhart signature. Suppression_requirement is authored because enforcement-capacity change IS this story's dynamic: the machinery builds to 0.74 by mid-interval, then decays to 0.52 as prosecutions grow costly and unproductive — a rise-and-fall arc rather than a cycle, so no intermittent-reinforcement reading applies. Accessibility_collapse stays at 0.48 because alternatives never closed: dual dating, private ritual, and old dress in domestic space remained available throughout, which is precisely this reading's failure mode. Resistance 0.63 reflects broad, diffuse, mostly uncoordinated non-compliance that nonetheless proved decisive: peasants never formed a coalition, but millions of small refusals outlasted the enforcement budget. One shared time grid (0/6/12/18/24/30) carries all three series; every metric is authored at every point. Claimed type tangled_rope is asserted from structure — a genuine coordination function (one civil timetable for taxation, contracts, and logistics) fused to asymmetric extraction (costs imposed without consultation) under active enforcement — not tuned to the metrics. Receipt surface: gains accrue to the decreeing state, so gain_flow names that seat; fixing was cheap in strict cost terms (repeal took a single act once enforcement yield fell) but was deferred for years by doctrinal face-saving, which is why the cheap-fix cell coexists with a long ceremonial tail.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as sovereign self-legislation: it wrote the statutes, reads its own compliance statistics, and sees decree working whenever filings conform. The payer seats meet the same statutes as uncompensated upheaval administered by strangers — a calendar that breaks lease dates, a dress rule that invites fines or street-level humiliation. Enforcement magistrates occupy a third position: the arrangement is their caseload and career ladder, so its persistence matters to them independently of whether displacement occurs. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the decreeing state (author and principal collector of compliance), urban administrative elites (careers flow through fluency in the mandated forms), and enforcement magistrates (prosecution counts are their currency). Victim declarations map to high directionality: rural peasant communities, parish clergy, and unconsulted rural women bear conversion, prosecution, and ritual-displacement costs under trapped or constrained exit. Provincial merchants sit between — listed among victims for the double-ledger years yet gaining something from eventual standardization — so a directionality override lifts the moderate power atom to 0.58 rather than letting the victim listing alone drive it toward full target; only one stakeholder holds that power atom, so the correction touches a single seat. Suppression is authored as a raw structural property and is not scaled by anything; extractiveness is what the engine scales by directionality and scope — national mandates verified village-by-village amplify effective extraction on scattered, immobile targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview returns a mismatch: the founding problem (instant, visible erasure of old-regime temporal and sartorial markers) is dead — conceded by the repealer's own 1806 message — while the disappearance verdict is world_rearranges, because administrative dating, festival budgets, and enforcement careers had organized themselves around the mandate. That dead-problem/live-arrangement pairing, cross-checked against theater_ratio crossing 0.5 late in the interval, is the zombie signature this corpus exists to catch. Classifying the arrangement as tangled_rope rather than snare preserves the genuine coordination achievement (a single civil timetable) that a pure-extraction coding would erase; refusing rope coding preserves the unconsulted, uncompensated cost structure that a pure-coordination coding would excuse. The mandate demonstrably outlived its function by interval end, surviving on ceremony until repeal removed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (exogenous_override) of the kernel legitimacy_of_imposed_practice; what structural differences would the sibling readings (endogenous_climb, hybrid_scaffolding) introduce if instantiated instead?',
    'Generate and classify the sibling reading files; compare epsilon, victim sets, and enforcement profiles across the three. The endogenous reading predicts measured compliance tracks internalization depth; the hybrid predicts compliance tracks messaging intensity alongside mandate.',
    'If the endogenous reading is correct, this file''s measured compliance is coerced formality and epsilon understates true failure; if the hybrid is correct, part of the observed displacement belongs to messaging infrastructure this reading cannot claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: which reading of the imposed-practice kernel the record supports.').

omega_variable(
    compliance_internalization_gap,
    'Does recorded compliance reflect genuine displacement of prior practice, or coerced formal adherence layered over continued private use?',
    'Private-record analysis: notarial dual-dating, parish registers, market-day records, and correspondence weighed against official compliance statistics.',
    'If adherence is formal-only, effective extraction is higher than measured (costs borne without the coordination benefit) and the reading''s sufficiency claim collapses toward the endogenous sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization_gap, empirical, 'Whether decree compliance was substantive or merely formal.').

omega_variable(
    theater_driver_ambiguity,
    'Is the rising theater_ratio driven by enforcement-capacity decay or by deliberate ceremonial substitution for failed displacement?',
    'Budget and personnel records for enforcement organs versus festival spending; if enforcement budgets fall while festival spending rises, substitution is deliberate rather than inertial.',
    'Deliberate substitution supports a managed-retreat reading (the arrangement retained for legitimacy display); capacity decay supports inertial drift toward purely performative maintenance before repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_driver_ambiguity, empirical, 'What drives the performative turn in late-episode enforcement.').

omega_variable(
    episode_generalization_limit,
    'Is epsilon for this reading episode-specific (calendar-scale failure) or doctrine-level — do sustained multi-generational enforcement campaigns, as in dress regimes, achieve partial displacement the short-mandate calendar cannot?',
    'Cross-episode comparison of displacement half-lives under comparable enforcement intensity, separating short-mandate from long-mandate impositions.',
    'If long-mandate episodes partially succeed, doctrine-level epsilon is lower than the calendar episode suggests and the reading retains a bounded validity zone rather than failing outright.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(episode_generalization_limit, conceptual, 'Scope boundary of the decree-sufficiency claim across imposition episodes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t6, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t18, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 18, 0.47).
narrative_ontology:measurement_basis(legi_tr_t18, observed).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(legi_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t6, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t18, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement_basis(legi_be_t18, observed).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(legi_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.56).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t6, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t18, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement_basis(legi_su_t18, observed).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(legi_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimacy of imposed practice' conflates three structurally distinct claims about how decree, internalization, and ideological reinforcement contribute to practice displacement; per the epsilon-invariance principle they are authored as separate files sharing one kernel. This file (exogenous_override) authors epsilon for the pure-decree arrangement; endogenous_climb authors epsilon for the internalization-necessity arrangement; hybrid_scaffolding authors epsilon for the reinforced-mandate arrangement. Upstream/downstream: the endogenous reading functions as the baseline claim that decree regimes are measured against, and the hybrid reading cites both siblings as its limiting cases. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__exogenous_override_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
