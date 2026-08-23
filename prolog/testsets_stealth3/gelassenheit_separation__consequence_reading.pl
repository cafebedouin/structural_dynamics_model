% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Ordnung Practice-Effect Evaluation Regime (Consequence Reading of Separation)
 *   domain: religious/technological/communal governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the gelassenheit_separation
 *   kernel: separation is maintained by preserving the community's practices,
 *   so each proposed technology is priced by what it does to visiting, mutual
 *   aid, and geographic rootedness. The standing arrangement under contest —
 *   and the sole referent of the authored epsilon — is the district rules'
 *   practice-effect evaluation regime itself: phone shanties at the lane's
 *   end but not phones in kitchens, tractors permitted as stationary belt
 *   power but not as self-propelled field machines, diesel compressors
 *   feeding pneumatic tools while grid lines stay out. The sibling readings
 *   (artifact_reading: forbid what resembles worldly artifacts regardless of
 *   function; principle_reading: admit whatever is functionally isolated from
 *   worldly systems) are separate constraints with their own epsilon,
 *   authored in their own files; they are not averaged into this one. Claim
 *   and metrics are independent authored facts: the arrangement is CLAIMED as
 *   tangled_rope because it possesses both a genuine coordination function
 *   and asymmetric cost-bearing under active enforcement, while the metrics
 *   are authored as descriptively true of its operation — moderately low
 *   extraction, moderate suppression, low theatricality. Where the engine's
 *   computed types diverge from the claim, that divergence is the datum.
 *
 * KEY AGENTS:
 *   - district_bishopric: agenda-setter (institutional / identity_locked) — convenes the twice-yearly review, adjudicates each device by practice-effect, enforces through discipline and avoidance
 *   - elderly_and_infirm_members: primary beneficiary (powerless / trapped) — anchored recipients of the care network the rules underwrite
 *   - farm_families: beneficiary with payer overlay (moderate / constrained) — draw cooperative labor, owe labor days back, forgo field mechanization
 *   - business_operating_members: primary payer with beneficiary overlay (organized / constrained) — bear compliance costs, win negotiated exemptions, trade on the community's trust mark
 *   - young_adults_in_formation: payer (powerless / mobile before baptism) — sample outside technology during the running-around years, bind at membership
 *   - shunned_former_members: excluded (powerless / relationally constrained) — object from outside a table they may no longer sit at
 *   - amish_studies_researchers: analytical observer — corroborate the founding problem and its status from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.45).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Ordnung Practice-Effect Evaluation Regime (Consequence Reading of Separation)").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technological/communal governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, 'a2a6aa4a-c3a7-4013-a231-6146c7763d00').
narrative_ontology:cs_kernel_codification('a2a6aa4a-c3a7-4013-a231-6146c7763d00', distributed).
narrative_ontology:cs_authority_grounding('a2a6aa4a-c3a7-4013-a231-6146c7763d00', lineage).
narrative_ontology:cs_interpretation_layer_present('a2a6aa4a-c3a7-4013-a231-6146c7763d00').
narrative_ontology:cs_reading_relation('a2a6aa4a-c3a7-4013-a231-6146c7763d00', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2a6aa4a-c3a7-4013-a231-6146c7763d00', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('a2a6aa4a-c3a7-4013-a231-6146c7763d00', foundational, technology_admissibility_tracks_communal_consequence).
narrative_ontology:cs_axiom_status(technology_admissibility_tracks_communal_consequence, holdable).
narrative_ontology:cs_axiom_grounding('a2a6aa4a-c3a7-4013-a231-6146c7763d00', technology_admissibility_tracks_communal_consequence, instrumental).
narrative_ontology:cs_axiom('a2a6aa4a-c3a7-4013-a231-6146c7763d00', secondary, admissibility_varies_by_context_of_use).
narrative_ontology:cs_axiom_status(admissibility_varies_by_context_of_use, holdable).
narrative_ontology:cs_axiom_grounding('a2a6aa4a-c3a7-4013-a231-6146c7763d00', admissibility_varies_by_context_of_use, instrumental).
narrative_ontology:cs_reference_frame('a2a6aa4a-c3a7-4013-a231-6146c7763d00', practice_preserving_separation).
narrative_ontology:cs_drift_state('a2a6aa4a-c3a7-4013-a231-6146c7763d00', contemporary_smartphone_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a2a6aa4a-c3a7-4013-a231-6146c7763d00', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_and_infirm_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, farm_families).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, business_operating_members).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, young_adults_in_formation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, business_operating_members).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, farm_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A bishop and two ministers, chosen by lot from the baptized men of the district, convene the congregation twice yearly to review the shared rules. When a member proposes a new device — a cell phone, a propane freezer, a cordless drill — they ask what it will do to visiting between households, to the habit of stopping work to raise a neighbor's barn, and to whether families stay on the land. They grant case-by-case permissions, announce them aloud, and discipline members who step outside the agreed terms, up to avoidance at the communion table. Nothing material is collected from the rules they administer; their standing consists in administering them.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, district_bishopric, agenda_setter,
    institutional, generational, identity_locked, regional).

% Widowed and aging members receive daily meals, night nursing after surgery, fire rebuilds, and harvest crews with no invoices and no premiums. The same arrangements that protect visiting and neighborly labor guarantee this care. Leaving the district would forfeit the only care network they hold. They do not vote in the twice-yearly meetings, but their needs are the standing reason given for the rules.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_and_infirm_members, beneficiary,
    powerless, biographical, trapped, local).

% Dairy and crop households draw on shared labor — threshing runs, silo filling, barn raisings — and owe their own work days back in return. They give up self-propelled field tractors and grid electricity that would let each household farm alone, keeping the crew economy intact. Selling out means leaving land, kin, and the labor exchange in one motion, so most stay and accept the terms, though the foregone equipment is a real cost they name at meeting time.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, farm_families, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, farm_families, payer).

% Cabinet shops, roofing crews, and quilt dealers sell far beyond the settlement. They run pneumatic tools off diesel compressors, hire outside drivers for long hauls, and share a phone shanty at the end of the lane instead of desk phones. Each accommodation is petitioned for and granted case by case, so the burden lands unevenly and the largest shops obtain the most tailored terms. Out-of-state buyers pay a premium for goods marked by the community's plainness, which returns part of what the rules cost them.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, business_operating_members, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__consequence_reading, business_operating_members, beneficiary).

% Before baptism, teenagers may work outside jobs, drive cars, and carry phones during their running-around years; the shared rules bind only at membership. Most return and join, accepting the terms in exchange for land access, marriage within the community, and a place in the labor network. Leaving for good before baptism is open, but it usually means distance from family and no trade beyond the one learned inside.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, young_adults_in_formation, payer,
    powerless, immediate, mobile, regional).

% Members who joined and later left, or who broke the terms and would not confess, are avoided at the table and in trade by relatives who remain. Many live within a few miles and observe the community's meetings from outside; several publish criticisms — that the rules are arbitrary, that avoidance punishes conscience — but none of it enters the twice-yearly deliberation, where only baptized members speak.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, shunned_former_members, excluded,
    powerless, biographical, constrained, national).

% Sociologists and historians of Anabaptism document the deliberations, count phone and engine adoption across settlements, and trace the affiliation splits that followed disagreements over how strictly to judge new devices. They hold no seat in any meeting; their studies are the main outside attestation of what the rules were built to solve and whether that problem persists.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, amish_studies_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the ratchet problem of communal-practice maintenance: if any household may freely adopt labor-saving, visit-displacing technology, each adoption lowers the marginal value of neighborliness for everyone else and the mutual-aid equilibrium unravels household by household. Binding all households to one shared evaluation — does this device erode visiting, mutual aid, or rootedness? — lets no one defect alone and keeps the crew economy, the care rotas, and the land-tied congregation intact.
% TRANSFER_FUNCTION: Moves discretion over technology from individual households to the district congregation, decided twice yearly and case by case. Moves labor and money through reciprocal channels — barn raisings, threshing rings, alms funds, care rotas — instead of purchased services. The price is paid in foregone productivity and compliance overhead, borne unevenly and heaviest by market-facing members.
% ABSENT_VOICES: Shunned and departed former members, unbaptized youth, outside spouses, and would-be converts have no seat in the deliberation; only baptized members speak. Their objections — arbitrariness of particular rules, the severity of avoidance — circulate as published criticism and family friction but never as votes, so the unanimity recorded at each half-year meeting is unanimity among those the rules already bind and benefit.
% DISAPPEARANCE_RATIONALE: If the evaluation regime vanished overnight, household-by-household adoption would ratchet immediately: phones migrate from lane-end shanties to kitchens, self-propelled tractors replace threshing crews, and each substitution lowers the next household's cost of defecting. Within a decade the mutual-aid demand that anchors care for the elderly would thin, visiting would move onto mediated channels, and the district would either reconstitute a successor rule or follow the documented path of affiliations that dropped the evaluation — assimilation within two to three generations.
% FOUNDING_PROBLEM: How can a covenant-bound, geographically rooted, mutually dependent community remain intact amid industrial technology that converts neighbors into customers, visits into calls, and cooperative labor into private machinery? The rules were built to answer that question for each device as it arrived.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Anabaptist historiography and sociology (settlement studies, adoption surveys, split histories) independently document both the founding threat and the adaptive response; longitudinal comparison with affiliations that abandoned the evaluation regime shows the predicted assimilation trajectory; defector memoirs attest the problem was real even while contesting the remedy. No attesting source sits inside the bishopric.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-moderate (0.28) because the fine-grained contextual rules price each device close to its actual practice-effect: a phone in the shanty preserves the shared-call norm at trivial cost, while the same phone in the kitchen would tax everyone's visiting, and the rules treat these as different cases. Suppression (0.45) reflects real enforcement machinery — confession, ban, avoidance — moderated by genuinely open exit and by the community's own amendment channel. Theater stays low (0.12) because the rules demonstrably produce the practices they name: barn raisings happen, care rotas fill, and the slow rise across the series tracks only the minority of provisions that persist as identity markers after their function faded. Accessibility_collapse (0.30) is low because understanding the rules does not close alternatives: members petition for exemptions, defect to progressive affiliates, or leave outright. Resistance (0.22) is low-moderate: periodic dissent, published criticism from the excluded, and historical splits, against broad acceptance grounded in the visible care network. The temporal series run on one shared grid (all three metrics at t=0,20,40,60,80,100); the suppression series carries a mid-interval hump — enforcement consolidated and hardened through the mid-century shunning controversies, then decayed as negotiated, case-by-case accommodation became the normal mode — while extraction declines gently as the contextual-permission repertoire matured. The series are humped and drifting, not oscillatory, so no intermittent-reinforcement cycle is claimed. Suppression is authored as raw structure and is not scaled by scope; only extractiveness is scaled, by the engine, through directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical rules. From the bishopric's seat the arrangement is the community's own self-government — near-zero effective extraction, possibly subsidy, since the rules constitute the administrators' standing. From the elderly seat it is nearly pure provision: care without invoices. From the business-member seat it is a real but discounted levy — compliance costs partially rebated through negotiated exemptions and the plainness premium their goods command. From the youth seat it is a dated constraint with a scheduled release valve. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: elderly members and farm families sit toward the beneficiary end; business members and formation-age youth toward the target end. Two overrides correct derivations the declarations alone would get wrong. Farm families (moderate) would derive a strongly beneficiary d from their primary beneficiary listing, yet they carry real levies — owed labor days, foregone mechanization — so their true position is mildly targetward of pure beneficiary (d=0.40). Business members (organized) would derive a near-full-target d from their victim listing, yet case-by-case exemptions, the trust premium on their goods, and access to the community's labor pool pull them substantially beneficiaryward (d=0.62). No override is needed for the bishopric (agenda-setter, derived low d is correct), the elderly (trapped beneficiaries, derivation correct), or youth (targets with a genuine exit valve the derivation reads correctly).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. As snare: the arrangement would need a capturer — but no seat monetizes the levy; the compliance costs and foregone productivity recycle into the commons (care rotas, barn raisings, alms funds) rather than accruing to any administrator, which is why gain_flow is authored as an affirmative 'diffuse' after checking every named seat. As piton: the receipt surface (prohibitive fixing cost plus diffuse gains) superficially resembles the piton cell, but the function is alive, theater_ratio is low, the founding problem is live, and the administrator actively exercises adaptive change every half-year rather than inertially maintaining a dead form — the opposite of the cost-asymmetry-and-performance profile. Mandatrophy is not resolved because the mandate has not outlived its function; the R5 interview records the founding problem as live with outside corroboration. Reading the arrangement as either predation or dead ritual would erase the actual structure: a working commons-management regime that charges real, unevenly distributed costs for a real, widely consumed good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separation_kernel_reading_indexicality,
    'This constraint instantiates the consequence_reading of the gelassenheit_separation kernel; the artifact_reading and principle_reading siblings instantiate different constraints over the same kernel, with different epsilon and different victim sets. Which evaluation criterion a settlement''s rules actually operationalize changes the classification entirely — where exactly is the disagreement located?',
    'Comparative authoring of the three sibling stories plus settlement-level coding of actual deliberation records: when a device is debated, is it argued from resemblance (artifact), from systemic entanglement (principle), or from effect on visiting, mutual aid, and rootedness (consequence)?',
    'Under the artifact_reading epsilon rises sharply (function-blind prohibition extracts regardless of effect); under the principle_reading epsilon falls (isolated-function technology admitted freely); the consequence_reading sits between with fine-grained contextual pricing. Epsilon is a property of the reading, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separation_kernel_reading_indexicality, conceptual, 'One-kernel-three-readings indexicality: this story authors epsilon for the consequence_reading only.').

omega_variable(
    suppression_sanction_vs_formation,
    'Is the measured suppression carried by the enforcement machinery (confession, ban, avoidance at the table) or by formation (an upbringing in yieldedness that makes members experience the rules as their own will)?',
    'Post-exit trajectory interviews comparing leavers and stayers: if reported experienced coercion jumps discontinuously at exit while the rules themselves did not change, a large share of the suppression was internalized rather than structural.',
    'If predominantly internalized, effective suppression on seated members is lower than the sanction apparatus suggests while true exit costs are higher than they appear; if structural, softening the machinery would visibly loosen the whole arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_sanction_vs_formation, empirical, 'Structural versus internalized suppression mechanism in a formed community.').

omega_variable(
    mutual_aid_net_valuation,
    'Does the preserved bundle of mutual aid, visiting, and rootedness actually return more welfare to members than the foregone technology and compliance costs it charges them?',
    'Welfare comparison against matched assimilated cohorts (former members, progressive-affiliation counterparts) on care outcomes, insurance value of the labor exchange, and revealed preference of leavers weighed against stayers'' revealed preference.',
    'A strongly positive valuation supports the coordination side of the arrangement; a negative one recasts it as extraction wearing coordination''s clothes and pushes classification toward the snare end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_aid_net_valuation, empirical, 'Whether the coordination good outweighs the price the arrangement charges for it.').

omega_variable(
    digital_absorption_ceiling,
    'Can the fine-grained contextual regime absorb always-on, networked technology (smartphones, internet commerce) the way it absorbed telephones and engines, or does digital media exceed what context-of-use rules can partition?',
    'Track the rules'' treatment of smartphones across settlements and the rate of affiliation-split events attributable to device disputes over the coming decade.',
    'Successful absorption keeps epsilon low and the hybrid coordination/extraction balance stable; failure would bifurcate the community into a permissive affiliate and a hardening remnant, raising epsilon on whichever side retains the governing machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_absorption_ceiling, empirical, 'Whether the consequence-evaluation regime scales to always-on media.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(gela_tr_t80, gelassenheit_separation__consequence_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(gela_tr_t100, gelassenheit_separation__consequence_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(gela_be_t80, gelassenheit_separation__consequence_reading, base_extractiveness, 80, 0.29).
narrative_ontology:measurement(gela_be_t100, gelassenheit_separation__consequence_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement(gela_su_t80, gelassenheit_separation__consequence_reading, suppression_requirement, 80, 0.49).
narrative_ontology:measurement(gela_su_t100, gelassenheit_separation__consequence_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% Colloquial 'Amish separation from the world' decomposes (epsilon-invariance) into three structurally distinct constraints over one kernel: the artifact_reading forbids by resemblance and extracts regardless of function (highest epsilon); this consequence_reading prices admission by practice-effect with fine-grained contextual rules (low-moderate epsilon); the principle_reading admits functionally isolated technology (lowest epsilon). All three descend from the same tradition and text-base; documented affiliation splits track disagreements over which criterion governs, so each reading's operation exerts legitimacy pressure on the others without logically eliminating any. This file links both siblings; each sibling links back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, moderate, 0.4).
constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
