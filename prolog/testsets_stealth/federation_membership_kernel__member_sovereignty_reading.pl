% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member-Sovereignty Bound on Free Movement Rights (Welfare Capacity and Labor Market Protection)
 *   domain: political economy / federalism / migration policy / welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the member_sovereignty_reading of the
 *   federation_membership_kernel: the claim that free movement within the EU
 *   is legitimate only insofar as national welfare state capacity and labor
 *   market protection can absorb it, and that member states retain authority
 *   to exclude economically inactive migrants in defense of social solidarity
 *   institutions. The epsilon referent is the standing arrangement under
 *   contest — the bounded-movement regime as it actually operates
 *   (habitual-residence tests, right-to-reside checks, Dano-line expulsion
 *   powers, the political economy of protection) — assessed by this reading's
 *   own lights: the reading endorses the arrangement yet authors non-trivial
 *   epsilon (0.58) because it acknowledges the costs as real prices of
 *   welfare autonomy — foregone mobility, conditional residence, selective
 *   demographic drain — rather than as nothing. Constraint family: the
 *   federation_membership_kernel decomposes into three readings with distinct
 *   epsilon values and victim sets — this reading (epsilon 0.58; victims
 *   include the excluded and the sending states), the integration_reading
 *   (unconditional citizenship rights; its own file), and the
 *   welfare_coordination_reading (anti-social-dumping coordination machinery;
 *   its own file). They are linked via network.affects_constraints, not
 *   merged: each instantiates a different constraint from the same treaty
 *   text. The claim/metric gap is deliberate: the reading is CLAIMED as
 *   tangled_rope (genuine bounded-pool coordination plus asymmetric, enforced
 *   extraction), and the metrics describe that operation independently — the
 *   engine computes per-seat classifications; the authored claim does not
 *   adjudicate them.
 *
 * KEY AGENTS:
 *   - member_state_governments: agenda-setter (institutional/constrained) — legislates and administers the boundary, collects political support from protection politics
 *   - receiving_state_welfare_institutions: primary beneficiary (institutional/constrained) — solidarity pools whose solvency the boundary protects
 *   - receiving_state_taxpayers: beneficiary (organized/constrained) — shielded contributor base, citizenship-bound to the national pool
 *   - economically_inactive_migrants: primary target (powerless/trapped) — excluded claimants facing residence denial and expulsion
 *   - mobile_eu_citizens: conditional-rights class (moderate/constrained) — retain movement but bear conditional status
 *   - sending_state_communities: diffuse target (moderate/constrained) — lose working-age members under selective mobility
 *   - sending_state_welfare_systems: diffuse target (institutional/constrained) — lose contributor cohorts, retain dependents
 *   - receiving_state_employers: cost-bearing seat with arbitrage exit (powerful/arbitrage) — tighter labor supply, relocation options
 *   - eu_court_of_justice: analytical observer (institutional/analytical) — adjudicates the boundary from the appellate seat
 *   - migrant_rights_advocates: excluded voice (organized/constrained) — contests the machinery without a Council seat
 *   - eu_commission: excluded voice (institutional/constrained) — holds the integrationist agenda without the votes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.63).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member-Sovereignty Bound on Free Movement Rights (Welfare Capacity and Labor Market Protection)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political economy / federalism / migration policy / welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5').
narrative_ontology:cs_kernel_codification('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', fixed_text).
narrative_ontology:cs_authority_grounding('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', lineage).
narrative_ontology:cs_interpretation_layer_present('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5').
narrative_ontology:cs_reading_relation('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', foundational, national_solidarity_requires_bounded_membership).
narrative_ontology:cs_axiom_status(national_solidarity_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', national_solidarity_requires_bounded_membership, deontological).
narrative_ontology:cs_axiom('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', secondary, movement_rights_conditional_on_contribution).
narrative_ontology:cs_axiom_status(movement_rights_conditional_on_contribution, holdable).
narrative_ontology:cs_axiom_grounding('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', movement_rights_conditional_on_contribution, instrumental).
narrative_ontology:cs_reference_frame('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', national_welfare_autonomy_settlement).
narrative_ontology:cs_drift_state('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', post_enlargement_contention, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4a924ed5-f87e-4e5e-89ea-babe1ac5ecc5', '2026-06-12T12:00:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_taxpayers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_eu_citizens).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_communities).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, receiving_state_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, mobile_eu_citizens).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, national_welfare_autonomy_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, bounded_solidarity_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, subsidiarity_in_social_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate and administer the residence conditions — habitual-residence tests, right-to-reside checks, benefit-eligibility rules — and negotiate in Council to preserve national welfare design autonomy. They collect political support from protection politics and retain the competence they defend. Exit is constrained: they are bound by the treaties they signed, and leaving the movement regime has so far meant leaving the Union.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, member_state_governments, beneficiary).

% Administer social insurance pools funded by nationally bounded contributor bases. The residence and economic-activity conditions keep each pool's obligations matched to its contributor base: claimants who have not contributed can be denied or removed. They do not write the rules, but their solvency is what the rules protect, and they supply the administrative data the tests run on.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions, beneficiary,
    institutional, generational, constrained, national).

% Fund the welfare pools through national taxation and payroll contribution. The boundary shields them from claims by people who contributed elsewhere; their exit is effectively nil — citizenship and tax residence bind them to the national pool, and they elect the governments that maintain it.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_taxpayers, beneficiary,
    organized, biographical, constrained, national).

% EU citizens who move without a job or sufficient resources: job-seekers past the grace period, carers, people whose work ended. They face denial of residence security and benefits and, under the Dano line, expulsion. Exit from their position is thin — return to the sending state, find work to convert status, or persist in precarity. Their horizon is immediate: housing and income now.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, continental).

% The broader class of citizens exercising movement rights. They retain the right to move and take up work, but their status is conditional: lose the job or the resources, lose the security. They carry the burden of documenting economic activity and live with residence that can be revisited whenever their circumstances change.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_eu_citizens, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, mobile_eu_citizens, beneficiary).

% Communities in Poland, Romania, Bulgaria, and the Baltics that lose working-age members under a regime that selects for the employable: those who move must be economically active, so the active leave and dependents stay. They cannot retain members whose departure the treaties guarantee, and they have no seat where the boundary rules are set.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_communities, payer,
    moderate, generational, constrained, national).

% National pension and health systems that lose contributor cohorts to out-migration while retaining the dependent populations the active leave behind. EU coordination rules bar them from discriminating against returnees or mobile citizens, and they have no standing to restrict their own citizens' departure.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Firms in migrant-dependent sectors — agriculture, care, construction, hospitality — that face tighter labor supply under the boundary. Their exit is comparatively strong: relocate production, automate, or win sectoral carve-outs; many do some combination.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_employers, payer,
    powerful, biographical, arbitrage, national).

% Adjudicates where movement rights end and welfare autonomy begins. Its Dano-line judgments upheld exclusion of economically inactive claimants; its earlier citizenship judgments expanded rights. It sees the full structure from the appellate seat and neither collects nor pays.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_court_of_justice, observer,
    institutional, generational, analytical, continental).

% NGOs, legal clinics, and transnational networks that contest the machinery: litigating residence tests, documenting expulsions, pressing the Commission and Parliament for expansive readings. They hold no seat in the Council where the boundary rules are negotiated.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, migrant_rights_advocates, excluded,
    organized, biographical, constrained, continental).

% Proposes expansive citizenship and movement legislation and brings infringement actions against restrictive member states. Its integrationist agenda holds the initiative seat but not the votes; Council bargaining leaves its proposals stalled, so it contests the boundary from outside the governing coalition.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, eu_commission, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_institutions).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps each national welfare pool's obligations matched to its nationally bounded contributor base: residence and economic-activity conditions define who may draw on a solidarity pool they have not contributed to, and labor market protection authority lets member states stage adjustment to migration flows. The coordination problem solved is the sustainability of bounded contributory welfare under legally guaranteed exit and entry.
% TRANSFER_FUNCTION: Moves welfare-system risk off receiving state solidarity pools and onto mobile citizens (who must self-insure by maintaining economic activity) and sending states (who retain dependents while losing contributor cohorts). Also moves regulatory authority over social protection from the supranational level back to member state governments, and political credit for protection to the governments that administer the boundary.
% ABSENT_VOICES: Economically inactive migrants have no seat anywhere the rules are set — not in Council, not in the national parliaments of the states that exclude them. Sending state governments attend Council, but their fiscal interest in out-migration is structurally unrepresented against receiving state protection coalitions. Migrant families and the future pension cohorts of sending states are absent entirely; their interests surface only through advocacy litigation.
% DISAPPEARANCE_RATIONALE: If the bounded regime vanished overnight, receiving state welfare systems would face immediate residence and benefit claims from mobile citizens without economic-activity filters; member states would lose a core sovereignty reservation and the political economy built on it; migration patterns would shift toward the integration reading's world — more family movement, more job-seeking moves with benefit backstops — and sending states would face accelerated departure of dependents as well as workers.
% FOUNDING_PROBLEM: When Maastricht decoupled movement rights from worker status (1992-93), it created citizens who could reside in a member state without ever contributing to its welfare pool. National welfare systems — built as bounded contributory solidarity — had no design for unbounded claimant populations. The founding problem: how to preserve national welfare solidarity once movement rights attach to citizenship rather than employment.
% FOUNDING_PROBLEM_CORROBORATION: The ECJ's own docket corroborates liveness from outside the benefiting parties: Brey (2013), Dano (2014), and Alimanovic (2015) exist because the collision is unresolved and litigated. Commission infringement statistics on residence-test discrimination and the academic social-dumping literature independently attest that the founding problem remains contested. No party claims the problem is solved; the disagreement is over the answer, not the problem.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 at interval end: the regime imposes real, rising costs on identifiable groups — excluded claimants, conditional-status mobile citizens, sending state demographics — while preserving a genuine coordination core (nationally bounded contributory welfare pools). Suppression (0.63) is a raw structural property, unscaled by power or scope: it measures the enforcement machinery's force against its targets (residence tests, benefit denials, expulsions), not the wielders' burden. Theater_ratio (0.46) reflects maintenance activity that is roughly half functional boundary-keeping and half symbolic protection politics outpacing measured cross-border fiscal flows (see the welfare_magnet_magnitude omega). Accessibility_collapse (0.50): alternatives persist — economically active movement remains open, sending states retain design autonomy, the coordination route exists — but the exclusion route for the inactive is substantially closed. Resistance (0.55): sustained litigation, sending state contest, Commission infringement action, and a mature migrant-rights bar; coalition potential among powerless claimants runs through the transnational advocacy network rather than direct collective bargaining. All three series run on one shared seven-point grid (T0 = 1993, Maastricht EU citizenship in force; T30 = 2023, the post-Dano/post-Brexit settlement) — every metric authored at every point, no metric borrowing another's end-state. The trajectories are monotonic (an enforcement ratchet, not a cycle): exclusion machinery built up through the enlargement era and hardened through the Dano line, with extraction accumulating on top of a coordination function that has not itself decayed. The coercion grid differentiates levels: the boundary's force lands on individuals (rising stakes, suppression, and closure at the individual level), is wielded rather than borne at the organizational level (member state governments face flat, low suppression), and meets persistent structural-level contest (the readings' institutional dispute).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats should compute different types from the same structural data. From member_state_governments and receiving_state_welfare_institutions, the arrangement is a legitimate sovereignty reservation they administer: bounded coordination with costs they judge justified. From economically_inactive_migrants (powerless, trapped, immediate horizon) and mobile_eu_citizens (conditional status), the same machinery operates as enforced exclusion with thin exit. Receiving_state_employers occupy a third position: they bear the reduced-flexibility cost with arbitrage-grade exit, so the constraint reads to them as a manageable supply problem rather than a boundary. The sending state seats experience a fourth structure: they set nothing, pay demographically, and cannot exit. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low d: receiving_state_welfare_institutions and receiving_state_taxpayers are subsidized by the boundary (their liability pools stay matched to their contributor bases), and member_state_governments collect retained authority plus political support — taxpayers' citizenship-bound exit keeps them near the beneficiary end despite ordinary tax burdens. Victims derive high d: economically_inactive_migrants (powerless, trapped, continental scope) sit nearest the full-target end, with the scope amplification applying across every member state they might claim in. Sending_state_communities and sending_state_welfare_systems bear diffuse demographic-fiscal costs with no exit and no agenda-setting power. Receiving_state_employers' arbitrage exit damps their d below the trapped victims'. Mobile_eu_citizens are dual-positioned: the derivation reads their victim declaration plus constrained exit toward high d; their residual movement right is real but conditional. No directionality override is authored: the override surface keys on power atom, and the story's two moderate-power seats (mobile_eu_citizens, sending_state_communities) sit at genuinely different d values, so a per-atom override would misapply to one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so mandatrophy is not resolved: the Maastricht-era collision between citizenship-attached movement and bounded contributory welfare is still litigated (the Dano line is recent in doctrinal terms) and still structures electoral politics — Brexit was fought substantially over this boundary. The tangled_rope claim prevents two mislabels: reading the regime as pure coordination would erase the asymmetric, enforced extraction the victim set documents; reading it as pure extraction would erase the genuine bounded-pool function that national welfare systems require under their current design (a function whose necessity is itself contested — see the bounded_solidarity_necessity omega). The theater_ratio series (0.22 to 0.46) tracks Goodhart drift in the protection justification — symbolic politics rising relative to measured fiscal exposure — but the enforcement machinery remains functional rather than merely performative, so no piton trajectory is claimed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federation_membership_kernel_reading_contestation,
    'This constraint is one reading of the federation_membership_kernel. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Institutional analysis of which seat holds exclusion authority under each reading: the integration_reading vests interpretive authority in supranational courts with unconditional citizenship rights (the victim set shrinks toward none and epsilon drops toward coordination cost); the welfare_coordination_reading replaces retained exclusion authority with anti-social-dumping coordination rules (the victim set shifts to rule-violating states and the extraction becomes enforcement cost). The disagreement is located in the legitimacy source of movement rights — supranational citizenship versus national welfare autonomy — and in who holds the boundary.',
    'If the integration reading displaced this one, economically_inactive_migrants leave the victim set and the constraint''s epsilon collapses toward the coordination floor; if the welfare_coordination reading displaced it, the exclusion machinery is replaced by coordination enforcement and the classification moves toward rope. This story''s classification is valid only for the member_sovereignty_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federation_membership_kernel_reading_contestation, conceptual, 'Committer-frame omega: this constraint is one reading of federation_membership_kernel; sibling readings would restructure the victim set and epsilon.').

omega_variable(
    welfare_magnet_magnitude,
    'How large are actual intra-EU welfare claims by economically inactive migrants relative to receiving state welfare budgets — does the magnet effect justify the boundary, or is the protection politics largely symbolic?',
    'Administrative data on cross-border benefit claims (Commission social-protection statistics, member state claimant records) compared against total welfare expenditure; natural experiments from the transitional-restriction episodes after the 2004 and 2007 enlargements.',
    'If claims are marginal (as most academic studies find), theater_ratio is understated and the constraint drifts toward pure extraction with protection politics as cover; if substantial, the coordination function is stronger and the tangled_rope reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_magnet_magnitude, empirical, 'Whether the welfare-protection justification tracks real fiscal exposure or symbolic politics.').

omega_variable(
    selective_brain_drain_compensation,
    'Does the bounded regime''s selectivity (only the economically active can move securely) intensify brain drain from sending states, and do compensating flows (remittances, return migration, EU structural funds) offset the loss?',
    'Demographic and fiscal-flow analysis of sending states: cohort loss by skill level, remittance and return-migration data, structural-fund transfers measured against contributor-base erosion.',
    'If uncompensated, sending_state_communities and sending_state_welfare_systems are confirmed victims and the victim set''s weight rises; if substantially compensated, their directionality falls toward symmetric and the extraction profile narrows to the directly excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_brain_drain_compensation, empirical, 'Whether the sending-state costs of selective exclusion are compensated or cumulative.').

omega_variable(
    bounded_solidarity_necessity,
    'Is bounded membership constitutively necessary for social solidarity (this reading''s foundational axiom), or can solidarity survive portable and contributory designs that decouple the welfare boundary from the national border?',
    'Comparative welfare-design analysis: portable benefit systems, hypothecated contributory accounts, and the welfare_coordination_reading''s anti-dumping architecture tested against solidarity indicators (redistribution support, tax compliance, contribution rates).',
    'If portable designs sustain solidarity, the exclusion authority is not necessary for the coordination function and the regime''s excess costs are unjustified even by this reading''s own lights; if bounded membership is constitutive, part of the measured cost is the price of the welfare state itself and the reading''s axiom holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bounded_solidarity_necessity, conceptual, 'Whether the solidarity boundary is constitutive or design-contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(fede_tr_t5, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(fede_tr_t15, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t25, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(fede_tr_t25, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement_basis(fede_be_t5, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(fede_be_t15, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t25, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(fede_be_t25, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(fede_su_t5, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(fede_su_t15, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t25, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement_basis(fede_su_t25, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(fede_su_t30, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=30
narrative_ontology:measurement(fede_grid_01, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(class), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_01, observed).
narrative_ontology:measurement(fede_grid_02, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(class), 30, 0.55).
narrative_ontology:measurement_basis(fede_grid_02, observed).
narrative_ontology:measurement(fede_grid_03, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_03, observed).
narrative_ontology:measurement(fede_grid_04, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(individual), 30, 0.6).
narrative_ontology:measurement_basis(fede_grid_04, observed).
narrative_ontology:measurement(fede_grid_05, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement_basis(fede_grid_05, observed).
narrative_ontology:measurement(fede_grid_06, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(organizational), 30, 0.45).
narrative_ontology:measurement_basis(fede_grid_06, observed).
narrative_ontology:measurement(fede_grid_07, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(structural), 0, 0.4).
narrative_ontology:measurement_basis(fede_grid_07, observed).
narrative_ontology:measurement(fede_grid_08, federation_membership_kernel__member_sovereignty_reading, accessibility_collapse(structural), 30, 0.55).
narrative_ontology:measurement_basis(fede_grid_08, observed).
narrative_ontology:measurement(fede_grid_09, federation_membership_kernel__member_sovereignty_reading, resistance(class), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_09, observed).
narrative_ontology:measurement(fede_grid_10, federation_membership_kernel__member_sovereignty_reading, resistance(class), 30, 0.55).
narrative_ontology:measurement_basis(fede_grid_10, observed).
narrative_ontology:measurement(fede_grid_11, federation_membership_kernel__member_sovereignty_reading, resistance(individual), 0, 0.25).
narrative_ontology:measurement_basis(fede_grid_11, observed).
narrative_ontology:measurement(fede_grid_12, federation_membership_kernel__member_sovereignty_reading, resistance(individual), 30, 0.45).
narrative_ontology:measurement_basis(fede_grid_12, observed).
narrative_ontology:measurement(fede_grid_13, federation_membership_kernel__member_sovereignty_reading, resistance(organizational), 0, 0.4).
narrative_ontology:measurement_basis(fede_grid_13, observed).
narrative_ontology:measurement(fede_grid_14, federation_membership_kernel__member_sovereignty_reading, resistance(organizational), 30, 0.5).
narrative_ontology:measurement_basis(fede_grid_14, observed).
narrative_ontology:measurement(fede_grid_15, federation_membership_kernel__member_sovereignty_reading, resistance(structural), 0, 0.45).
narrative_ontology:measurement_basis(fede_grid_15, observed).
narrative_ontology:measurement(fede_grid_16, federation_membership_kernel__member_sovereignty_reading, resistance(structural), 30, 0.5).
narrative_ontology:measurement_basis(fede_grid_16, observed).
narrative_ontology:measurement(fede_grid_17, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_17, observed).
narrative_ontology:measurement(fede_grid_18, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(class), 30, 0.55).
narrative_ontology:measurement_basis(fede_grid_18, observed).
narrative_ontology:measurement(fede_grid_19, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement_basis(fede_grid_19, observed).
narrative_ontology:measurement(fede_grid_20, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(individual), 30, 0.65).
narrative_ontology:measurement_basis(fede_grid_20, observed).
narrative_ontology:measurement(fede_grid_21, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(organizational), 0, 0.25).
narrative_ontology:measurement_basis(fede_grid_21, observed).
narrative_ontology:measurement(fede_grid_22, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(organizational), 30, 0.4).
narrative_ontology:measurement_basis(fede_grid_22, observed).
narrative_ontology:measurement(fede_grid_23, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(structural), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_23, observed).
narrative_ontology:measurement(fede_grid_24, federation_membership_kernel__member_sovereignty_reading, stakes_inflation(structural), 30, 0.5).
narrative_ontology:measurement_basis(fede_grid_24, observed).
narrative_ontology:measurement(fede_grid_25, federation_membership_kernel__member_sovereignty_reading, suppression(class), 0, 0.25).
narrative_ontology:measurement_basis(fede_grid_25, observed).
narrative_ontology:measurement(fede_grid_26, federation_membership_kernel__member_sovereignty_reading, suppression(class), 30, 0.4).
narrative_ontology:measurement_basis(fede_grid_26, observed).
narrative_ontology:measurement(fede_grid_27, federation_membership_kernel__member_sovereignty_reading, suppression(individual), 0, 0.35).
narrative_ontology:measurement_basis(fede_grid_27, observed).
narrative_ontology:measurement(fede_grid_28, federation_membership_kernel__member_sovereignty_reading, suppression(individual), 30, 0.65).
narrative_ontology:measurement_basis(fede_grid_28, observed).
narrative_ontology:measurement(fede_grid_29, federation_membership_kernel__member_sovereignty_reading, suppression(organizational), 0, 0.15).
narrative_ontology:measurement_basis(fede_grid_29, observed).
narrative_ontology:measurement(fede_grid_30, federation_membership_kernel__member_sovereignty_reading, suppression(organizational), 30, 0.15).
narrative_ontology:measurement_basis(fede_grid_30, observed).
narrative_ontology:measurement(fede_grid_31, federation_membership_kernel__member_sovereignty_reading, suppression(structural), 0, 0.3).
narrative_ontology:measurement_basis(fede_grid_31, observed).
narrative_ontology:measurement(fede_grid_32, federation_membership_kernel__member_sovereignty_reading, suppression(structural), 30, 0.5).
narrative_ontology:measurement_basis(fede_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: federation_membership_kernel decomposes into three readings with distinct epsilon values and victim sets. This member_sovereignty_reading authors epsilon 0.58 over the bounded-movement regime, with victims {economically_inactive_migrants, mobile_eu_citizens, sending_state_communities, sending_state_welfare_systems, receiving_state_employers}. The integration_reading authors its own epsilon over the unconditional-rights arrangement it endorses; the welfare_coordination_reading over the coordination-machinery arrangement. The readings are linked, not merged: each instantiates a different constraint from the same treaty text, and this reading's retained-exclusion authority creates downstream pressure on the coordination architecture (coordination rules such as Regulation 883/2004 exist because member states insisted on welfare design autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
