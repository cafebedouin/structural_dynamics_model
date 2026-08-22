% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership — Integration Reading (Free Movement as Constitutional Right)
 *   domain: political economy/federalism/migration
 *
 * SUMMARY:
 *   The standing arrangement under contest is the federation-membership
 *   regime as it actually operates: membership treated as irreversible
 *   integration, supranational institutions holding legitimate authority over
 *   movement, and free movement of persons enforced as a constitutional right
 *   of citizens rather than a negotiable policy. This story instantiates the
 *   INTEGRATION READING of the federation_membership kernel, authored from
 *   that reading's own seat. Per the epsilon-referent rule, epsilon is
 *   authored for the standing arrangement — the free-movement regime itself —
 *   never for the compensated, deeper-integration variant the reading would
 *   endorse. Even from the integrationist seat, the regime's operation
 *   imposes uncompensated displacement: labor flows along the wage gradient
 *   concentrate adjustment costs on immobile residents and municipal budgets
 *   in receiving regions while the gains concentrate with mobile citizens and
 *   cross-gradient employers. Hence the declared structure: mobile citizens
 *   and employers in the beneficiary set, local labor markets and service
 *   authorities in the victim set, high base extractiveness driven by labor
 *   displacement, and border restriction treated as illegitimate within this
 *   reading. The claim/metric gap is deliberate and independent: the reading
 *   CLAIMS tangled_rope (genuine continental coordination entangled with
 *   asymmetric extraction) while the metrics describe what the regime's
 *   operation actually looks like; the engine computes per-seat types from
 *   the structural data and the divergence, if any, is the measurement. This
 *   file is one member of a two-story constraint family; the sibling
 *   federation_membership__sovereignty_reading instantiates the opposing
 *   reading with its own epsilon, victim set, and classification.
 *
 * KEY AGENTS:
 *   - supranational_institutions: Agenda setter (institutional/identity_locked) — proposes, enforces, and adjudicates the mobility rules; constituted by the project it administers
 *   - member_state_governments: Dual-positioned agenda setter and payer (institutional/constrained) — retains formal treaty power, absorbs compliance costs and voter backlash
 *   - multinational_employers: Primary concentrated beneficiary (powerful/arbitrage) — captures the wage-arbitrage surplus of cross-gradient hiring
 *   - mobile_eu_citizens: Diffuse beneficiary (organized/mobile) — exercises and litigates the movement right
 *   - receiving_region_local_workers: Primary target (powerless/trapped) — bears wage competition and housing pressure without ability to move
 *   - municipal_service_authorities: Secondary target with partial offset (organized/constrained) — delivers services under arrival pressure, partly compensated by structural funds
 *   - sending_region_households: Mixed beneficiary (moderate/constrained) — gains remittances, loses working-age cohorts
 *   - third_country_nationals: Excluded voice (powerless/trapped) — lives under the tiered mobility order with no seat in setting it
 *   - national_constitutional_courts: Analytical observer (analytical/analytical) — adjudicates supremacy conflicts, shifting the environment for all seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.74).
domain_priors:suppression_score(federation_membership__integration_reading, 0.55).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership — Integration Reading (Free Movement as Constitutional Right)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '1533a800-ab92-45c0-b0c2-26760d020e90').
narrative_ontology:cs_kernel_codification('1533a800-ab92-45c0-b0c2-26760d020e90', fixed_text).
narrative_ontology:cs_authority_grounding('1533a800-ab92-45c0-b0c2-26760d020e90', lineage).
narrative_ontology:cs_interpretation_layer_present('1533a800-ab92-45c0-b0c2-26760d020e90').
narrative_ontology:cs_reading_relation('1533a800-ab92-45c0-b0c2-26760d020e90', federation_membership__sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('1533a800-ab92-45c0-b0c2-26760d020e90', foundational, membership_irreversibility).
narrative_ontology:cs_axiom_status(membership_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('1533a800-ab92-45c0-b0c2-26760d020e90', membership_irreversibility, conventional).
narrative_ontology:cs_axiom('1533a800-ab92-45c0-b0c2-26760d020e90', foundational, free_movement_constitutional_right).
narrative_ontology:cs_axiom_status(free_movement_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('1533a800-ab92-45c0-b0c2-26760d020e90', free_movement_constitutional_right, deontological).
narrative_ontology:cs_axiom('1533a800-ab92-45c0-b0c2-26760d020e90', secondary, supranational_primacy_doctrine).
narrative_ontology:cs_axiom_status(supranational_primacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('1533a800-ab92-45c0-b0c2-26760d020e90', supranational_primacy_doctrine, conventional).
narrative_ontology:cs_reference_frame('1533a800-ab92-45c0-b0c2-26760d020e90', ever_closer_union_telos).
narrative_ontology:cs_drift_state('1533a800-ab92-45c0-b0c2-26760d020e90', post_withdrawal_populist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1533a800-ab92-45c0-b0c2-26760d020e90', '').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, sending_region_households).
narrative_ontology:constraint_victim(federation_membership__integration_reading, receiving_region_local_workers).
narrative_ontology:constraint_victim(federation_membership__integration_reading, municipal_service_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, municipal_service_authorities).
narrative_ontology:constraint_victim(federation_membership__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce the common rules of membership: propose mobility legislation, bring infringement actions against governments that restrict movement, and adjudicate through the court the meaning of citizenship and residence rights. Their budget, staffing, and legitimacy are constituted by the integration project they administer; there is no vantage point outside it from which they could operate.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, generational, identity_locked, continental).

% Signed the founding treaties and retain formal amendment power by unanimity, but day-to-day rules are set by qualified majority and reviewed by the supranational court. They implement mobility rules domestically, absorb the political backlash of voters affected by them, and can leave only through a multi-year withdrawal process carrying severe economic cost, as one member has demonstrated.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, member_state_governments, payer).

% Recruit across the entire continental labor pool, filling shortages in high-wage regions with workers from lower-wage ones and moderating wage growth in tight markets. Operations can be relocated between jurisdictions, which gives them leverage in any national dispute over mobility rules.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, multinational_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% Exercise residence and employment rights in any member state: cross-border commuters, seasonal workers, students, professionals, retirees. The rights are personally enforceable in the supranational court; their practical exit option is the mobility itself, and advocacy organizations litigate to defend it.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_eu_citizens, beneficiary,
    organized, biographical, mobile, continental).

% Live and work in the districts where inward mobility concentrates: they compete for jobs and housing with newcomers, see wage growth in their sectors lag, and cannot themselves move — skills, family ties, property, and language bind them in place. They vote nationally, but the rules that bind them are set above the national level.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, receiving_region_local_workers, payer,
    powerless, biographical, trapped, regional).

% Deliver housing, schooling, primary care, and registration services wherever population arrives, without proportional fiscal compensation; some also receive structural funds and population-linked revenue that partially offset the load. Their remedy runs through national governments, not through the supranational level.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, municipal_service_authorities, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership__integration_reading, municipal_service_authorities, beneficiary).

% Receive remittances and widened opportunity for members who move abroad, but watch working-age cohorts depart: villages thin, care burdens fall on those left behind, and local economies lose consumers and workers. Their stake is genuinely mixed — household income rises while the demographic base erodes.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, sending_region_households, beneficiary,
    moderate, generational, constrained, regional).

% Live and work inside the federation under national permits, quotas, and fees that Union citizens bypass entirely. They perform much of the labor in the same receiving-region markets but hold none of the movement rights and have no forum in which the tiered order is negotiated.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, third_country_nationals, excluded,
    powerless, biographical, trapped, continental).

% Adjudicate conflicts between national constitutional identity and supranational law, reviewing whether mobility and citizenship rules exceed what national ratification authorized. They take no side in the underlying dispute, but their rulings shift the operating environment for every other seat.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_constitutional_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, multinational_employers).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates and maintains a single continental labor market: mutual recognition of qualifications, portable social-security rights, and enforceable residence rights solve the reciprocity and collective-action problem that bilateral treaties handle poorly, and supranational institutions adjudicate disputes uniformly instead of dyad by dyad.
% TRANSFER_FUNCTION: Moves labor from lower-wage to higher-wage regions; moves wage-bargaining leverage from immobile receiving-area workers toward mobile workers and the employers who hire across the gradient; moves adjustment costs — housing demand, service loads, sectoral wage competition — onto immobile residents and municipal budgets; and moves regulatory authority over movement from national parliaments to supranational courts and commissions.
% ABSENT_VOICES: Immobile residents of receiving communities and the municipalities that serve them have no direct seat: they reach the table only filtered through national governments already committed to the integration project. Third-country nationals live inside the mobility order's tiers with no voice in setting them. Both would object to uncompensated cost-bearing and to the two-tier structure if admitted to the conversation.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand millions of cross-border workers, students, and retirees in legal limbo; seasonal agriculture, health systems, universities, and construction dependent on mobile labor would reorganize within months; sending regions would lose remittance income; and the remaining freedoms of the single market would face immediate legal uncertainty, since movement of persons anchors the others.
% FOUNDING_PROBLEM: Post-war Western European fragmentation: recurring nationalist mobilization, closed national economies, and the fear that restored sovereign border control would reproduce the conditions of interwar conflict. Integration was designed to make reversal unthinkable by entangling populations — the functional spillover logic of the founders.
% FOUNDING_PROBLEM_CORROBORATION: Historians and the peace-research literature — outside the benefiting parties — corroborate the founding problem's historical reality: the interwar breakdown and the war are documented independently of any integrationist advocacy. Whether the problem remains live is disputed: security scholars citing renewed territorial aggression on the continent attest liveness, while political scientists documenting democratic-control losses attest that the problem has mutated rather than persisted. No seat inside the beneficiary set is relied upon for either half of the attestation.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the regime's gains and costs are asymmetrically distributed: the mobility right generates real surplus, but the adjustment costs land on seats with the least capacity to bear or avoid them, and no compensating mechanism is built into the right itself. Suppression is moderate (0.55) and structural rather than personal: no individual is coerced, but national border and residence policies toward Union citizens are legally foreclosed once inside, and the enforcement machinery (infringement actions, primacy rulings) actively strikes down member-state attempts to restore them. Theater ratio is moderate-low (0.30): the mobility function is real and heavily used, but a growing share of activity is ceremonial integration rhetoric — summits, anniversary declarations, ever-closer-union invocations — that substitutes for delivered deepening. Accessibility collapse is 0.58: unilateral alternatives collapse for insiders, but the Swiss-model bilateral path and the Article 50 exit door persist as costly outside options. Resistance is 0.62: a completed member withdrawal, repeated referendum movements, national opt-outs, and sustained electoral backlash constitute real, ongoing resistance that the regime must continuously answer. The measurement series run on ONE shared time grid (points 0, 5, 10, 15, 20, 25, 30, mapping approximately to 1993-2023 in five-year steps) with every tracked metric authored at every point: base extractiveness climbs with successive enlargements that steepened the wage gradient crossed by mobile labor; suppression requirement rises as the enforcement infrastructure matured through infringement practice and citizenship jurisprudence, then plateaus as the machinery reaches steady state; theater ratio rises gradually as summitry outpaces substantive deepening. No cyclical oscillation is asserted — the trajectories are monotonic. On coalition: the principal target seat is dispersed, low-salience, and electorally channeled through national parties committed to the regime, so coalition power remains latent rather than effective; this is noted as context, not tuned into any metric. Suppression here is a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the supranational agenda-setter seat the regime is a constitutional achievement it is institutionally fused with — its identity_locked exit means it cannot even frame the arrangement as optional. From the mobile-citizen and employer seats the same structure is liberty and labor-market depth. From the trapped receiving-region worker seat it is uncompensated exposure to competition it never consented to and cannot exit; from the municipal seat it is an unfunded mandate with partial offsets. Member-state governments straddle: formally co-authors of the rules, practically payers of the political cost. The engine derives these divergent per-seat classifications from power, exit, and directional position; this commentary only explains why they must diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive mobile_eu_citizens, multinational_employers, and sending_region_households toward the beneficiary end of d; victim declarations drive receiving_region_local_workers and municipal_service_authorities toward the target end. Exit modulation sharpens this: the trapped receiving-region workers sit nearest the full-target pole (no arbitrage, no mobility — the constraint's costs are unavoidable from where they stand), while the arbitrage-grade exit of multinational_employers damps their effective burden despite their beneficiary position, and the mobile citizens' exit IS the right itself. One explicit override is authored: sending_region_households sit at the moderate power atom, and the structural derivation from their beneficiary declaration would place them near d = 0.15-0.20; that is wrong, because their remittance gains arrive bundled with demographic drain — departing workers, accelerating aging, hollowed local economies — making their true position near-symmetric at d = 0.40. The override is surgical because no other stakeholder occupies the moderate power atom. Municipal_service_authorities need no override: their dual declaration (payer with beneficiary secondary role) already places them near symmetric, which matches their mixed fiscal position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents misclassification in both directions. Reading the regime as pure coordination (rope) would erase the identifiable victims — the trapped receiving-region workers and unfunded municipalities — and license indefinite expansion of the displacement. Reading it as pure extraction (snare) would erase the genuine coordination function — credential recognition, portable social security, uniform adjudication — whose loss would injure the very seats now bearing the costs, and would misread the founding problem, which is at least partly live. The mandate has not outlived its function: enforcement is active, the mobility function is heavily used, and the founding problem retains contested liveness, so neither piton decay nor scaffold sunset applies. The correct reform surface the classification exposes is compensatory: the extraction component is addressable through fiscal-transfer and adjustment mechanisms attached to the mobility right, not through abolishing the right — which is precisely the distinction a tangled_rope reading forces and a snare or rope reading would obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (integration_reading) of the contested kernel federation_membership; what would the sibling sovereignty_reading change structurally, and where exactly is the disagreement located?',
    'Comparative analysis against the sibling story federation_membership__sovereignty_reading: the sibling relocates the victim set (communities seeking border control become protected rather than exposed), shifts the extraction referent to supranational overreach rather than labor displacement, and re-authors epsilon over a different standing arrangement.',
    'If the sovereignty reading were adopted as the operative frame, the beneficiary/victim sets invert for the border dimension, epsilon''s referent changes, and the classification of the membership arrangement recomputes from scratch; the two stories must never be merged into one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of a two-reading kernel; the sibling reading is a separate constraint file.').

omega_variable(
    displacement_attribution_share,
    'What share of receiving-region wage stagnation, housing pressure, and service strain is causally attributable to intra-federation mobility, as opposed to automation, fiscal austerity, and housing-supply policy?',
    'Natural-experiment designs around the 2004 enlargement wave and the transitional-control periods some members negotiated: difference-in-differences on wage and service-load series between exposed and shielded regions during the control window.',
    'If the attributable share is small, base extractiveness falls toward rope territory and the regime reads as coordination with incidental friction; if large, the extraction component dominates and the profile drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_attribution_share, empirical, 'Causal share of observed receiving-region costs owed to the mobility regime itself.').

omega_variable(
    irreversibility_depth,
    'Is membership practically irreversible, or does the demonstrated withdrawal path show the irreversibility premise to be aspirational rather than structural?',
    'Observe subsequent withdrawal, renegotiation, and suspension episodes: whether individual acquired rights survive state-level exit (the acquired-rights doctrine suggests they do), and whether re-entry terms punish departure.',
    'If exit is genuinely cheap and rights-preserving, the enforcement asymmetry weakens, accessibility_collapse falls, and the arrangement drifts toward the sibling reading''s conditional-treaty structure; if exit is ruinous, the irreversibility premise holds and the current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_depth, empirical, 'Whether the irreversibility premise is structural fact or maintained fiction.').

omega_variable(
    adjustment_compensation_adequacy,
    'Do structural and cohesion fund flows into receiving regions adequately offset the displacement costs the mobility regime imposes on them?',
    'Fiscal-incidence studies netting transfers against quantified displacement costs region by region, over a full funding cycle.',
    'Full offset would recode much of the measured extraction as coordination cost and pull the classification toward rope; partial offset confirms the tangled_rope profile; negligible offset would push the profile toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_compensation_adequacy, empirical, 'Whether the regime''s compensatory flows neutralize its extractive component.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__integration_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fede_tr_t5, federation_membership__integration_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(fede_tr_t10, federation_membership__integration_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fede_tr_t15, federation_membership__integration_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(fede_tr_t20, federation_membership__integration_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(fede_tr_t25, federation_membership__integration_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(fede_tr_t30, federation_membership__integration_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__integration_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(fede_be_t5, federation_membership__integration_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(fede_be_t10, federation_membership__integration_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(fede_be_t15, federation_membership__integration_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(fede_be_t20, federation_membership__integration_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t25, federation_membership__integration_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(fede_be_t30, federation_membership__integration_reading, base_extractiveness, 30, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__integration_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t5, federation_membership__integration_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(fede_su_t10, federation_membership__integration_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(fede_su_t15, federation_membership__integration_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(fede_su_t20, federation_membership__integration_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(fede_su_t25, federation_membership__integration_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(fede_su_t30, federation_membership__integration_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'federation membership' conflates two structurally distinct constraints held by different parties. This file instantiates the integration reading (free movement as constitutional right; epsilon authored over the standing mobility regime with labor-displacement victims). The sibling federation_membership__sovereignty_reading instantiates the sovereignty reading (border control as national prerogative; epsilon authored over supranational authority with democratic-control victims). The epsilon values differ widely, the victim sets differ, and the failure modes differ; they are linked here as family members, upstream-downstream in discourse (integrationist jurisprudence is cited as evidence in sovereignty debates), never merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__integration_reading, moderate, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
