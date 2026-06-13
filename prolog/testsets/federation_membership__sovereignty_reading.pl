% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political_economy/federalism/migration
 *
 * SUMMARY:
 *   Federation membership under the sovereignty reading frames free movement
 *   as a negotiable policy outcome, not a constitutional entitlement.
 *   National governments retain the legitimate authority to set immigration
 *   policy, control borders, and negotiate mobility terms bilaterally or
 *   within treaty frameworks. Federation membership is conditional—a state
 *   can suspend it, renegotiate terms, or impose new restrictions if
 *   labor-market or political conditions demand it. This reading stands in
 *   structural opposition to the integration reading, which treats federation
 *   membership as irreversible and mobility as constitutional right. The
 *   sovereignty reading benefits destination-country labor markets and border
 *   authorities by legitimating labor-market gatekeeping; it extracts from
 *   mobile citizens and origin-country workers by making their access
 *   contingent and negotiable. Extractiveness has risen over 1995–2025 as
 *   labor-market pressures increased and political movements pushed for
 *   stricter border enforcement, while theater ratio has risen as the
 *   functional coordination problem has been substantially solved by economic
 *   convergence and standardized law, leaving enforcement machinery
 *   disproportionately defending the extraction mechanism.
 *
 * KEY AGENTS:
 *   - National labor market protectors (institutional beneficiary): domestic governments, employers, and labor unions protecting local wage levels and employment composition through border control
 *   - Border control authorities (agenda-setter): national immigration services that negotiate federation terms and enforce differential access
 *   - Mobile citizens (payer, identity-locked): federation members whose cross-border access depends on permits and negotiated agreements
 *   - Cross-border workers (payer, powerless): workers from lower-wage member states facing quota restrictions and work-permit conditionality
 *   - Supranational federation bureaucracy (excluded): federation courts and institutions that would argue for constitutional mobility rights
 *   - Origin-country governments (observer, powerful): lower-wage states that negotiate federation terms but cannot override national gatekeeping
 *   - Analytical observer (observer, analytical): measures the structural extraction and its rise over time as the founding problem erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.67).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.71).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '49f2093f-d136-49ea-ab4d-782eeeef482b').
narrative_ontology:cs_kernel_codification('49f2093f-d136-49ea-ab4d-782eeeef482b', fixed_text).
narrative_ontology:cs_authority_grounding('49f2093f-d136-49ea-ab4d-782eeeef482b', lineage).
narrative_ontology:cs_interpretation_layer_present('49f2093f-d136-49ea-ab4d-782eeeef482b').
narrative_ontology:cs_reading_relation('49f2093f-d136-49ea-ab4d-782eeeef482b', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('49f2093f-d136-49ea-ab4d-782eeeef482b', foundational, national_border_authority_retained).
narrative_ontology:cs_axiom_status(national_border_authority_retained, holdable).
narrative_ontology:cs_axiom_grounding('49f2093f-d136-49ea-ab4d-782eeeef482b', national_border_authority_retained, deontological).
narrative_ontology:cs_axiom('49f2093f-d136-49ea-ab4d-782eeeef482b', foundational, federation_membership_conditional_on_labor_terms).
narrative_ontology:cs_axiom_status(federation_membership_conditional_on_labor_terms, holdable).
narrative_ontology:cs_axiom_grounding('49f2093f-d136-49ea-ab4d-782eeeef482b', federation_membership_conditional_on_labor_terms, deontological).
narrative_ontology:cs_reference_frame('49f2093f-d136-49ea-ab4d-782eeeef482b', treaty_based_federation_governance).
narrative_ontology:cs_drift_state('49f2093f-d136-49ea-ab4d-782eeeef482b', contemporary_supranational_mobility_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('49f2093f-d136-49ea-ab4d-782eeeef482b', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_labor_market_protectors).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, border_control_authorities).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments and domestic labor constituencies benefit from the constraint's legitimation of border control and labor-market gatekeeping. They retain the authority to negotiate federation membership terms and can impose conditions on free movement, protecting domestic wages and employment levels from external competition. The constraint vindicates the principle that federation membership is conditional, not irreversible.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_labor_market_protectors, beneficiary,
    institutional, generational, mobile, national).

% National immigration authorities set and enforce the boundary between members and non-members, negotiate mobility agreements bilaterally or within federation framework, and retain discretion over admission, residence, and work permits. They administer the treaty framework and decide whether specific provisions are honored or suspended. Their legitimacy depends on the sovereignty reading: they represent the national will in negotiating federation terms.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, border_control_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Citizens seeking to work, study, or reside across federation borders face conditional, negotiable access rather than constitutional right. Their mobility depends on bilateral agreements, work permits, residency restrictions, and border authorities' discretionary decisions. Exit looks like remaining in their origin country or pursuing work outside the federation entirely. Their identity as federation members does not automatically grant them access—access is contingent on national labor-market assessment and political will.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, identity_locked, continental).

% Workers from less-developed member states or candidate states face the highest friction. They depend on employer sponsorship, work permit allocation (often limited by quota), and renewal contingent on labor-market conditions. They bear the cost of mobility restrictions through wage suppression in origin labor markets and exclusion from higher-wage destinations. They cannot claim right of access; access is gift-like, contingent, and reversible.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, biographical, constrained, continental).

% Federation-level institutions that would argue for mutual recognition of professional credentials, standardized mobility rights, and supranational adjudication of disputes are structurally sidelined by the sovereignty reading. They are excluded from setting the terms; their role is to administer national decisions, not to override them. A supranational court that ruled mobility a constitutional right would challenge the entire framework.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_federation_bureaucracy, excluded,
    institutional, generational, trapped, continental).

% Governments of lower-wage member or candidate states observe the constraint from outside the main distribution. They negotiate federation terms and can withdraw (exit) if terms become too unfavorable. They face asymmetric pressure: their citizens want mobility, but the federation frames that mobility as conditional and negotiable. They can lobby for quota increases or mutual recognition agreements, but they cannot claim rights—only bargaining positions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, origin_country_governments, observer,
    powerful, generational, mobile, national).

% Analytical seat: examines the structural relationship between federation membership (the kernel), the sovereignty reading (treaty-based, negotiable), and the integration reading (constitutional, irreversible). Measures how the sovereignty reading instantiates extraction via mobility restriction and legitimates that extraction as national prerogative.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_coordination_observer, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, border_control_authorities).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates labor-market access, professional credential recognition, and border administration across federation members. Solves the genuine problem of managing movement across jurisdictions with different wage levels, welfare systems, and labor standards without centralized authority deciding allocation.
% TRANSFER_FUNCTION: Transfers bargaining power from mobile citizens and origin-country governments to destination-country governments and border authorities. Moves control of labor-market composition, credential recognition, and work-permit allocation from supranational rules to national negotiation. Transfers the benefit of labor-cost differential from workers (in the form of wage convergence) to destination labor markets and employers (who can restrict wage pressure).
% ABSENT_VOICES: Supranational federation institutions (courts, commission bodies) that would argue for constitutional mobility rights are structurally excluded from the sovereignty reading's frame. Citizens in origin countries with no representation in destination-country labor markets or political institutions would argue for movement rights but have no seat at negotiation tables. They are excluded by geography and power asymmetry.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading and its enforcement vanished—if free movement became constitutional rather than negotiable—origin-country workers would flow to higher-wage destinations over years, destination labor markets would face wage convergence and political reaction, origin countries would lose working-age population and remittance flows, and federation membership would become irreversible rather than conditional. The political economy of the federation would reorganize around integration rather than negotiated treaties.
% FOUNDING_PROBLEM: Federation members faced the problem of coordinating movement and labor standards across borders without surrendering national authority over labor-market composition, welfare-system access, and cultural integration. The sovereignty reading solved this by treating federation membership as a conditional treaty—members retain border legitimacy and can negotiate terms bilaterally or within framework agreements.
% FOUNDING_PROBLEM_CORROBORATION: National governments and border authorities attest the founding problem is live: uncontrolled movement would burden welfare systems and strain labor markets. Supranational courts, human-rights bodies, and origin-country governments attest the founding problem has been substantially solved by economic development and standardized labor law, and the constraint persists as extraction of bargaining power. Academic research on wage convergence and labor-market integration shows the founding rationale has eroded in high-development zones.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.67 at interval end) reflects the constraint's mechanism: it restricts the supply of labor to destination markets by conditioning mobility on national negotiation, raising destination wages above competitive equilibrium. Suppression (0.71) is high because the constraint depends on sustained enforcement of border control, visa regimes, and work-permit systems—alternatives (free movement, labor-market integration) are actively suppressed through law and enforcement capacity. Theater ratio (0.42) is moderate-high because the founding coordination problem (managing cross-border movement and credential recognition) has been substantially solved by economic development and standardized EU/bilateral labor law; the remaining enforcement effort defends the gatekeeping mechanism rather than solving coordination failures. The measurement series show rising extractiveness and theater ratio over 1995–2025: as wage convergence reduced the coordination rationale, destination countries tightened enforcement (responding to political pressure), making the extraction mechanism more visible and theater more pronounced. Suppression requirement has also risen as resistance from mobile populations and origin-country governments has grown.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty reading computes differently from the agenda-setter seat (border authorities, national governments) and the victim seats (mobile citizens, cross-border workers). From the agenda-setter position, the arrangement is legitimate treaty-based negotiation protecting national labor-market interests. From the victim position, the same arrangement is enforced mobility restriction where exit (leaving the federation entirely) is identity-locked because citizenship and origin-country social networks anchor identity. The engine computes this divergence from directionality atoms: agenda-setters are near the beneficiary end (d ~0.2), mobile citizens are identity-locked targets (d ~0.85), cross-border workers are constrained victims (d ~0.78). The supranational observer seat is excluded from the frame entirely—from the integration reading seat, the sovereignty reading appears as extraction legitimated by treaty doctrine; from the sovereignty reading seat, the integration reading appears as illegitimate usurpation of national authority.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and border authorities benefit from the constraint (they control gatekeeping and bargaining leverage) and are highly mobile in institutional terms—they can renegotiate, suspend, or withdraw from federation if terms become unfavorable. This places them near d=0.1–0.3. Mobile citizens and cross-border workers are the targets: they depend on permits for access and cannot exit by claiming federation rights (the sovereignty reading denies those rights). Their exit is identity-locked (they are federation citizens, but citizenship does not grant movement)—they would have to leave the federation entirely and lose social networks, language, family. This places them near d=0.75–0.85. The supranational federation bureaucracy is excluded rather than coordinated—from the sovereignty reading's perspective, they have no legitimate voice in setting terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty reading avoids false classification by declaring both a real coordination function (managing cross-border movement) and asymmetric extraction (mobility restriction protecting destination labor markets). The claimed type is tangled_rope: it has both genuine coordination (solving the problem of credential recognition and border administration across jurisdictions) and extraction (restricting labor supply to protect destination wages). Active enforcement (border control, visa regimes, work-permit systems) is required to hold the extraction mechanism in place. The founding problem was live in 1995 (free movement was chaotic in newly expanded federations); it is contested now (economic development and standardization have solved most coordination failures, but gatekeeping persists). The rising theater ratio signals that the arrangement is increasingly performing gatekeeping rather than solving coordination—a piton trajectory. However, it is not yet a piton because beneficiaries (destination governments) still actively maintain it with substantial investment, not from inertia. The constraint is a tangled_rope under rising extraction pressure, approaching piton-hood only if enforcement machinery becomes primarily theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_market_convergence_obsolescence,
    'Has economic development and wage convergence within the federation made the founding labor-market gatekeeping problem obsolete, or does it remain live?',
    'Measure actual wage gaps between destination and origin labor markets over time, and test whether remaining gaps are sustained by the constraint''s gatekeeping or by other factors (productivity, education, infrastructure). Track labor flow if gatekeeping were removed: if movement remains below levels predicted by wage gaps, the constraint is still active.',
    'If obsolete, the constraint has shifted from coordination (solving a real cross-border coordination problem) to pure extraction (gatekeeping with no coordination rationale). The rising theater ratio would signal this transition toward piton-hood. If live, the coordination function still justifies some extraction as coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_convergence_obsolescence, empirical, 'Whether the founding problem the sovereignty reading solved has been eliminated by economic development.').

omega_variable(
    identity_lock_mechanism_supranational_pressure,
    'Is the identity-lock experienced by mobile citizens (they are federation members but cannot claim mobility as right) structurally embedded in national identity, or is it maintained by the enforcement framing itself?',
    'Post-policy natural experiments: what happens to identity fusion if a country formally recognizes mobility rights? Do mobile citizens'' allegiance to origin country dissolve or remain stable? Track self-identification changes in populations that gain versus lose freedom of movement.',
    'If structurally embedded, identity-lock is a genuine suppression mechanism that persists even if mobility rules change—the constraint has internalized. If maintained by framing, removal of gatekeeping rules would dissolve it, revealing that suppression was enforcement-dependent, not internalized. This changes the suppression score and the exit-options classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_supranational_pressure, empirical, 'Whether identity-lock in cross-border movement is intrinsic or sustained by the constraint''s framing.').

omega_variable(
    integration_reading_ascendance_contradiction,
    'If the integration reading (supranational authority, constitutional mobility) gains institutional and political dominance, does the sovereignty reading become foreclosed, or do they coexist as competing commitments?',
    'Monitor supranational court rulings on mobility rights; track federation-level legislation asserting constitutional mobility; observe whether national governments can still opt out without losing federation membership.',
    'If integration reading becomes institutional law and national governments lose gatekeeping authority, the sovereignty reading''s foundational axiom (national authority over borders) would be foreclosed—they would no longer coexist. The sovereignty reading would shift from live institutional commitment to repudiated doctrine. If they coexist in law (some rights supranational, some negotiable), they remain competitors in a distributed kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_reading_ascendance_contradiction, conceptual, 'Whether the two readings of federation membership can coexist institutionally or will eventually foreclose one another.').

omega_variable(
    theater_ratio_rise_causation,
    'Is the rising theater ratio (0.18 to 0.42 from 1995 to 2025) driven by the founding coordination problem being solved, or by political populism amplifying gatekeeping enforcement beyond what coordination would require?',
    'Decompose enforcement activity into coordination-necessary (credential verification, fraud detection, document validation) and gatekeeping-only (discretionary visa refusal, quota enforcement, deportation campaigns). Track the ratio of each category over time.',
    'If coordination-necessary enforcement has flattened or declined while gatekeeping-only enforcement has risen, theater_ratio accurately reflects the shift from coordination to extraction. If both rise in tandem, gatekeeping is still partly justified by coordination. This affects piton-classification: pure performance → piton-adjacent; coordinated + gatekeeping → tangled_rope still legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_rise_causation, empirical, 'Whether rising theater reflects solved coordination or amplified gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1995, federation_membership__sovereignty_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(fede_tr_t1995, observed).
narrative_ontology:measurement(fede_tr_t2003, federation_membership__sovereignty_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement_basis(fede_tr_t2003, observed).
narrative_ontology:measurement(fede_tr_t2011, federation_membership__sovereignty_reading, theater_ratio, 2011, 0.32).
narrative_ontology:measurement_basis(fede_tr_t2011, observed).
narrative_ontology:measurement(fede_tr_t2018, federation_membership__sovereignty_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement_basis(fede_tr_t2018, observed).
narrative_ontology:measurement(fede_tr_t2025, federation_membership__sovereignty_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(fede_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1995, federation_membership__sovereignty_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(fede_be_t1995, observed).
narrative_ontology:measurement(fede_be_t2003, federation_membership__sovereignty_reading, base_extractiveness, 2003, 0.52).
narrative_ontology:measurement_basis(fede_be_t2003, observed).
narrative_ontology:measurement(fede_be_t2011, federation_membership__sovereignty_reading, base_extractiveness, 2011, 0.59).
narrative_ontology:measurement_basis(fede_be_t2011, observed).
narrative_ontology:measurement(fede_be_t2018, federation_membership__sovereignty_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement_basis(fede_be_t2018, observed).
narrative_ontology:measurement(fede_be_t2025, federation_membership__sovereignty_reading, base_extractiveness, 2025, 0.67).
narrative_ontology:measurement_basis(fede_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1995, federation_membership__sovereignty_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(fede_su_t1995, observed).
narrative_ontology:measurement(fede_su_t2003, federation_membership__sovereignty_reading, suppression_requirement, 2003, 0.59).
narrative_ontology:measurement_basis(fede_su_t2003, observed).
narrative_ontology:measurement(fede_su_t2011, federation_membership__sovereignty_reading, suppression_requirement, 2011, 0.65).
narrative_ontology:measurement_basis(fede_su_t2011, observed).
narrative_ontology:measurement(fede_su_t2018, federation_membership__sovereignty_reading, suppression_requirement, 2018, 0.69).
narrative_ontology:measurement_basis(fede_su_t2018, observed).
narrative_ontology:measurement(fede_su_t2025, federation_membership__sovereignty_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(fede_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership__sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% The federation_membership kernel supports two structurally distinct readings: sovereignty_reading (this constraint) frames federation as conditional treaty with national gatekeeping; integration_reading (sibling) frames federation as irreversible with supranational mobility rights. These readings share the same kernel but instantiate different ε values and beneficiary structures. The sovereignty reading extracts from mobile citizens via mobility restriction; the integration reading would eliminate that extraction by making mobility a right. The readings coexist as live political positions across federation member states and in competing institutional frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership__sovereignty_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
