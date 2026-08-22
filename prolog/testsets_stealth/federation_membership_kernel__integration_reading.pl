% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement as Constitutive Citizenship Right (Integration Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   Since Maastricht, free movement has been framed as a fundamental right
 *   constitutive of Union citizenship rather than an economic privilege, and
 *   the Court of Justice has read the scope expansively: residence and
 *   equal-treatment claims decoupled from economic activity, social benefits
 *   extended to students and job-seekers, and national labor-protection
 *   instruments struck down where they impeded mobility. The arrangement
 *   genuinely coordinates - it allocates labor across formerly closed markets
 *   and completes the single market's factor mobility - while concentrating
 *   real costs on identifiable seats: exposed local labor segments,
 *   receiving-state welfare budgets without compensating transfers, and
 *   sending-state services depleted of publicly trained staff. This file
 *   instantiates ONLY the integration reading of the
 *   federation_membership_kernel; the sibling readings are separate
 *   constraints with their own epsilon and victim sets. Epsilon's referent is
 *   the standing arrangement - the movement regime as operated under this
 *   reading's jurisprudence - assessed by this reading's own lights, which
 *   regard the arrangement as largely rightful coordination that nonetheless
 *   bears real, unevenly distributed costs. Claim and metrics are authored
 *   independently: the tangled_rope claim is my structural judgment; the
 *   metric values are my descriptive judgment of how the arrangement actually
 *   operates.
 *
 * KEY AGENTS:
 *   - european_court_of_justice: Agenda-setter (institutional/identity_locked) - final interpreter of movement scope; each ruling redefines the arrangement's reach
 *   - european_commission: Agenda-setter (institutional/constrained) - enforces the expansive reading against member states via infringement
 *   - mobile_eu_workers: Primary beneficiary (moderate/mobile) - hold portable residence and equal-treatment claims
 *   - cross_border_employers: Secondary beneficiary (powerful/arbitrage) - capture the wage and flexibility spread of a continent-wide labor pool
 *   - exporting_member_states: Dual-positioned (moderate/constrained) - collect remittances and unemployment relief while their public services lose trained staff
 *   - displaced_local_workers: Primary target (powerless/trapped) - compete in exposed labor segments without geographic or skill mobility
 *   - receiving_state_taxpayers: Cost bearer (powerless/trapped) - fund welfare access extended to arrivals without compensating transfers
 *   - receiving_state_governments: Cost-bearing administrator (institutional/constrained) - run the absorbing welfare systems under rules they cannot unilaterally alter
 *   - sending_state_public_services: Target (organized/trapped) - health and technical services depleted by emigration of trained staff
 *   - national_trade_unions: Excluded voice (organized/constrained) - labor-protection interests overridden by primacy in posted-worker and collective-action cases
 *   - comparative_political_economists: Analytical observer (analytical/analytical) - map the fiscal-incidence and displacement evidence all seats selectively cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.62).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.66).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement as Constitutive Citizenship Right (Integration Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'ea9c0857-4117-44d4-830f-cfd2bee30718').
narrative_ontology:cs_kernel_codification('ea9c0857-4117-44d4-830f-cfd2bee30718', fixed_text).
narrative_ontology:cs_authority_grounding('ea9c0857-4117-44d4-830f-cfd2bee30718', lineage).
narrative_ontology:cs_interpretation_layer_present('ea9c0857-4117-44d4-830f-cfd2bee30718').
narrative_ontology:cs_reading_relation('ea9c0857-4117-44d4-830f-cfd2bee30718', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea9c0857-4117-44d4-830f-cfd2bee30718', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('ea9c0857-4117-44d4-830f-cfd2bee30718', foundational, free_movement_constitutive_of_union_citizenship).
narrative_ontology:cs_axiom_status(free_movement_constitutive_of_union_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('ea9c0857-4117-44d4-830f-cfd2bee30718', free_movement_constitutive_of_union_citizenship, deontological).
narrative_ontology:cs_axiom('ea9c0857-4117-44d4-830f-cfd2bee30718', secondary, supranational_expansive_interpretive_authority).
narrative_ontology:cs_axiom_status(supranational_expansive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('ea9c0857-4117-44d4-830f-cfd2bee30718', supranational_expansive_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('ea9c0857-4117-44d4-830f-cfd2bee30718', expansive_market_citizenship_frame).
narrative_ontology:cs_drift_state('ea9c0857-4117-44d4-830f-cfd2bee30718', post_brexit_rule_of_law_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ea9c0857-4117-44d4-830f-cfd2bee30718', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, exporting_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, displaced_local_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_taxpayers).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_public_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, exporting_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Answers preliminary references on the scope of movement and equal-treatment rights and strikes down national measures that condition access on economic activity or lengthy residence. Its docket grows with each accession and is dominated by disputes over where national welfare discretion ends. Departing from its established interpretive line would require overturning decades of its own precedent and its self-understanding as guarantor of the rights it created.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_court_of_justice, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Monitors member-state compliance with movement rules, opens infringement proceedings against restrictive national practices, and negotiates transitional arrangements for new accessions. It proposes the directives that define what counts as a lawful restriction. Its leverage depends on keeping the member governments it polices cooperative on unrelated files.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Move to where wages and vacancies are, carrying residence and equal-treatment entitlements that do not depend on any single state's consent. Relocating again if conditions deteriorate is the option that defines their position. They bear moving costs, credential-recognition friction, and status precarity during job search.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Recruit across the whole Union labor pool, filling shortages in destination regions and moderating wage growth in exposed sectors. Large firms shift hiring and production between jurisdictions to arbitrage labor costs and rules. Beyond ordinary taxes wherever they operate, they contribute nothing scheme-specific while capturing the wage-and-flexibility spread of a continent-wide market.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, global).

% See unemployment pressure and household poverty relieved as workers depart, and receive remittance inflows that support consumption back home. At the same time their hospitals, care systems, and technical services lose staff trained at public expense. They cannot tax the destination earnings of departed workers and cannot rebuild lost professional cohorts quickly.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, exporting_member_states, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, exporting_member_states, payer).

% Work in construction, food processing, agriculture, and care - the segments where posted and settled incoming labor concentrates. Wage growth in these segments stagnates and job security erodes; retraining programs exist but presume capacities many do not have. Moving abroad is not realistic for them, which is precisely why the competition lands where it does.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, displaced_local_workers, payer,
    powerless, immediate, trapped, national).

% Fund the schools, clinics, housing allowances, and child benefits extended to newly arrived workers and their families. Whether the ledger balances depends on employment rates and local concentration; the costs arrive locally and immediately while offsetting contributions are diffuse and lagged. They hold no procedural seat in the arrangement that extends the benefits.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_taxpayers, payer,
    powerless, biographical, trapped, national).

% Administer the welfare systems that absorb arrival costs and face the electoral consequences of perceived strain. Treaty rules and court rulings remove most instruments they would otherwise use to manage inflows; their remaining levers are registration formalities and habitual-residence tests that litigation repeatedly narrows. Leaving the arrangement entirely carries costs on the scale of unwinding half a century of economic integration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, receiving_state_governments, agenda_setter).

% Lose physicians, nurses, engineers, and tradespeople to destination-country recruitment, often mid-career after publicly funded training. Replacement pipelines take a decade or more; rural facilities close or ration care. They have no voice in the destination-side rules that shape the outflow.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_public_services, payer,
    organized, generational, trapped, national).

% Organized labor in destination countries litigated against posted-worker arrangements and lost landmark cases establishing that collective action can itself constitute a restriction. Their protective institutions - sectoral bargaining, posting standards - are progressively overridden by higher-ranking rules. They continue organizing inside a legal frame they did not set and cannot revise.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, national_trade_unions, excluded,
    organized, biographical, constrained, national).

% Study fiscal incidence, displacement, and brain-drain magnitudes across member states and publish the evidence base that every other seat selectively cites. They hold no stake in the arrangement's continuation or termination, and their findings cut in different directions for different seats.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, comparative_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, cross_border_employers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates labor across twenty-seven previously closed national markets toward where it is most productive, makes residence and work rights portable without bilateral negotiation, and gives firms a continent-wide hiring pool - problems no member state could solve alone.
% TRANSFER_FUNCTION: Moves workers, and their claims on schools, clinics, and benefits, from lower-wage to higher-wage regions; moves adjustment costs onto receiving-state welfare budgets and exposed local labor segments; moves remittances and vacancy relief back to sending states.
% ABSENT_VOICES: Displaced local workers and destination-country unions had no seat when the citizenship provisions and the expansive case law consolidated - their interests entered only as litigated afterthoughts. Sending-state public services likewise never negotiated the outflow their staffing absorbs. They stand outside both national ratification politics and the Court's docket except as respondents.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand millions of cross-border workers and families, break staffing in destination health and care systems within weeks, sever remittance flows supporting households in exporting regions, and force renegotiation of posted-worker and qualification-recognition regimes - labor markets, welfare administration, and firm location choices would all reorganize.
% FOUNDING_PROBLEM: Post-war Western Europe needed to make renewed war materially impossible by entangling national economies, and the common market required factors of production - including people - to move as freely as goods. Free movement of persons was built as the third freedom completing that design.
% FOUNDING_PROBLEM_CORROBORATION: Historians of European integration, working from Schuman- and Monnet-era records and independent of today's benefiting parties, corroborate the founding problem as stated. What no one outside the integrating institutions attests is that the founding problem still requires unrestricted person mobility rather than the bounded or coordinated variants the sibling readings propose - that extension rests on the Commission's and Court's own assertions.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 reflects costs that are real but concentrated: exposed labor segments see stagnant wages, receiving-state budgets absorb arrivals without compensating transfers, and exporting states lose publicly trained staff - while the same arrangement delivers broad mobility gains that keep participation voluntary for those who move. Suppression 0.66 is legal-coercive and unscaled by scope or power in the engine's arithmetic: primacy and infringement foreclose national restrictive instruments, and the posted-worker line converts collective defense into a reviewable restriction. Theater 0.28: citizenship rhetoric outruns practice (transitional controls delayed promised equality for up to seven years per accession; habitual-residence tests qualify equal treatment), but screening, recognition, and portability functions are genuinely performed. Accessibility collapse 0.45: within the regime, national alternatives are struck down, yet the Brexit route demonstrates exit exists at prohibitive cost and welfare-design autonomy survives in parallel. Resistance 0.55: referendum reversals, national constitutional-court pushback, and sustained union litigation are real but have redirected rather than stopped the expansive line. All three temporal series share one time grid (points 0-30) so no metric borrows another's endpoint; the rising trajectories track successive enlargements and the accumulation of scope-expanding case law.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's and Commission's seats the arrangement is a political community arriving at its logical constitutional form; from displaced workers' and taxpayers' seats it is an unfunded mandate enforced from above; from exporting treasuries it is relief, from exporting hospitals it is depletion. Same nominal level - member governments - splits by net migration position: destination governments experience constraint, origin governments experience subsidy-with-leakage. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (mobile workers, employers, exporting states) derive low d; declared victims (displaced workers, receiving taxpayers, sending-state public services) derive high d, amplified by trapped exits. Employers' arbitrage-grade exit pushes them toward the beneficiary pole despite paying only ordinary taxes. Exporting states carry a genuine internal split - beneficiary treasury, victim services - captured by declaring the services as a separate victim seat rather than averaging the government's position. Receiving-state governments are deliberately left out of both arrays: their net position is the live empirical question, and their payer role is recorded on the stakeholder surface instead of forced into a structural declaration the evidence does not settle.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim preserves both halves of the structure. The coordination function explains why twenty-seven states ratified and voluntarily sustain the arrangement - a fact a pure-extraction reading renders mysterious - while the victim declarations preserve who pays, which a pure-coordination reading would erase. Mandatrophy is not resolved: the founding problem's modern extension remains contested, and the mismatch consumer will find status=contested paired with verdict=world_rearranges, a coherent pairing rather than a zombie flag. The arrangement's mandate (war-proofing through interdependence) has arguably been achieved, but no successor consensus exists on what should replace the mobility guarantee, so the arrangement persists on live contest rather than dead function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the integration reading of the federation_membership_kernel - would the member_sovereignty_reading or welfare_coordination_reading produce a different victim set and epsilon for the same treaty text?',
    'Author the sibling reading stories and compare computed classifications; the disagreement is located in who holds final bounding authority over movement and welfare access.',
    'Under the sovereignty reading, displaced_local_workers and receiving_state_taxpayers recede as victims and member-state exclusion capacity becomes the protected structure; under the coordination reading the fiscal-transfer channel becomes the primary surface. Epsilon and type are reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is one of three readings of the membership kernel.').

omega_variable(
    net_fiscal_incidence,
    'What is the net fiscal incidence of intra-EU migration on receiving-state welfare systems once age and employment composition are controlled?',
    'Fiscal incidence studies using administrative microdata, disaggregated by member state and by locality of settlement.',
    'A net-positive contribution weakens the receiving_state_taxpayers victim seat and shifts classification toward rope; concentrated uncompensated local costs strengthen measured extraction and push toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_fiscal_incidence, empirical, 'Whether receiving-state welfare costs are compensated by migrant contributions.').

omega_variable(
    displacement_attribution,
    'Does incoming mobile labor displace resident workers or fill vacancies residents decline, and how does the answer vary by skill segment and locality?',
    'Quasi-experimental labor studies exploiting staggered accession timing and transitional-control variation across member states.',
    'If displacement is small, displaced_local_workers becomes a contested rather than confirmed victim seat and measured extraction falls; if displacement concentrates in specific segments, extraction rises despite aggregate neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_attribution, empirical, 'Causal status of the local-labor displacement claim.').

omega_variable(
    brain_drain_net_externality,
    'Is the net effect on exporting member states positive (remittances, returnee skills, unemployment relief) or negative (depleted medical and technical public services)?',
    'Country-panel studies correlating emigration rates with public-service staffing levels, remittance inflows, and returnee human capital.',
    'Determines whether exporting_member_states'' secondary payer role dominates its beneficiary role, altering its derived directionality and the symmetry of the exchange.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_net_externality, empirical, 'Net position of sending states in the mobility exchange.').

omega_variable(
    court_identity_lock,
    'Is the Court''s consistently expansive interpretation driven by doctrinal obligation within the treaty framework, or by institutional identity fusion with the integration project?',
    'Analysis of internal deliberation records, advocate-general opinions, and judicial behavior in cases where retreat carried low reputational cost.',
    'If identity-fused, the arrangement persists through institutional self-conception rather than functional demand - a decay signature if mobility''s coordination value falls; if doctrinal, the arrangement tracks the treaty text''s own logic and is stable while the text stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(court_identity_lock, conceptual, 'Source of the Court''s interpretive constancy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__integration_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__integration_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__integration_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__integration_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__integration_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__integration_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__integration_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__integration_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__integration_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__integration_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__integration_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__integration_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__integration_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__integration_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__integration_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__integration_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__integration_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__integration_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__integration_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__integration_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__integration_reading, suppression_requirement, 30, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU free movement' decomposes into three structurally distinct constraints sharing one treaty kernel: this file (integration_reading - expansive rights, supranational final interpretive authority), member_sovereignty_reading (national bounding authority and exclusion capacity), and welfare_coordination_reading (inter-system coordination preserving welfare design autonomy). Their epsilon values differ because their victim sets differ: this reading counts displaced local labor, receiving-state taxpayers, and sending-state public services; the sovereignty reading counts member-state autonomy itself; the coordination reading treats fiscal effects as a transfer-design problem rather than naming victims. Each file stands alone with a single stable epsilon; the edges here express family kinship, not shared measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
