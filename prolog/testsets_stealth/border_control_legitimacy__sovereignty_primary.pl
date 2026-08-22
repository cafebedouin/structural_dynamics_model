% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Sovereign Admission Discretion — Constitutive Border Control Reading
 *   domain: political philosophy/international law/migration studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the contested
 *   kernel border_control_legitimacy: the claim that state territorial
 *   sovereignty entails absolute discretion to exclude non-citizens and that
 *   border control is constitutive of statehood itself. The standing
 *   arrangement under contest is the modern exclusion-and-enforcement regime
 *   built on that claim; epsilon is authored through this reading's own
 *   lights, which regard exclusion as legitimate collective
 *   self-determination rather than wrongful taking — hence a moderate-low
 *   base extractiveness despite a large enforcement apparatus. The
 *   claim/metric gap is deliberate: the reading CLAIMS mountain
 *   (constitutive, natural to statehood) while the authored structural data
 *   names beneficiaries and victims, so the false-summit signature will
 *   evaluate whether 'constitutive of statehood' is natural law or a
 *   constructed arrangement dressed as one. The expected structural delta is
 *   realized in the data: excluded migrants and refused asylum seekers sit in
 *   the victim set, the enforcement apparatus is justified internally as
 *   sovereignty defense, and human-rights limits are treated as external
 *   constraints on the authority rather than constitutive of it. Sibling
 *   readings are separate constraints with their own epsilon values over the
 *   same referent; they are not described, hedged, or averaged here.
 *
 * KEY AGENTS:
 *   - receiving_state_executives: agenda-setter and principal collector ([institutional]/[identity_locked]) — administers admission discretion; political identity fused with the border-authority claim
 *   - national_citizenries: primary beneficiary ([organized]/[mobile]) — membership scarcity, wage and welfare-pool protection, reciprocal exit mobility
 *   - border_security_contractors: secondary beneficiary ([organized]/[arbitrage]) — enforcement budgets, surveillance and detention markets sold across regimes
 *   - excluded_migrants: primary target ([powerless]/[trapped]) — bears routes, smuggler debt, detention, deportation
 *   - refused_asylum_seekers: target at the limit-case ([powerless]/[trapped]) — bears refoulement risk where the external limits go unenforced
 *   - transit_zone_communities: displaced cost-bearer ([moderate]/[constrained]) — hosts externalized processing and pushback corridors
 *   - destination_economy_employers: excluded seat ([organized]/[arbitrage]) — wants labor channels, absent from the legitimation forums
 *   - human_rights_treaty_bodies: analytical observer ([institutional]/[analytical]) — treats the qualifying limits as inherent, contra this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.38).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.62).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.38).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, mountain).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Sovereign Admission Discretion — Constitutive Border Control Reading").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political philosophy/international law/migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).
domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '11e14c5c-78a9-476d-bcd2-99bb6c53f597').
narrative_ontology:cs_kernel_codification('11e14c5c-78a9-476d-bcd2-99bb6c53f597', formalized).
narrative_ontology:cs_authority_grounding('11e14c5c-78a9-476d-bcd2-99bb6c53f597', lineage).
narrative_ontology:cs_interpretation_layer_present('11e14c5c-78a9-476d-bcd2-99bb6c53f597').
narrative_ontology:cs_reading_relation('11e14c5c-78a9-476d-bcd2-99bb6c53f597', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('11e14c5c-78a9-476d-bcd2-99bb6c53f597', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('11e14c5c-78a9-476d-bcd2-99bb6c53f597', foundational, admission_discretion_is_absolute_and_constitutive).
narrative_ontology:cs_axiom_status(admission_discretion_is_absolute_and_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('11e14c5c-78a9-476d-bcd2-99bb6c53f597', admission_discretion_is_absolute_and_constitutive, conventional).
narrative_ontology:cs_axiom('11e14c5c-78a9-476d-bcd2-99bb6c53f597', secondary, self_determination_requires_member_controlled_admission).
narrative_ontology:cs_axiom_status(self_determination_requires_member_controlled_admission, holdable).
narrative_ontology:cs_axiom_grounding('11e14c5c-78a9-476d-bcd2-99bb6c53f597', self_determination_requires_member_controlled_admission, deontological).
narrative_ontology:cs_reference_frame('11e14c5c-78a9-476d-bcd2-99bb6c53f597', classical_westphalian_admission_discretion).
narrative_ontology:cs_drift_state('11e14c5c-78a9-476d-bcd2-99bb6c53f597', contemporary_human_rights_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('11e14c5c-78a9-476d-bcd2-99bb6c53f597', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, national_citizenries).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, receiving_state_executives).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_security_contractors).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, refused_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, transit_zone_communities).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, constitutive_border_control_theory).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, collective_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate and administer admission rules, command the enforcement agencies, and sign the externalization agreements with transit states. Their statecraft, rhetoric, and political survival are fused with the border-authority claim itself; relinquishing absolute discretion would require redefining what the state is, not merely adjusting a policy. Enforcement budgets, emergency powers, and symbolic capital flow through their offices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, receiving_state_executives, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, receiving_state_executives, beneficiary).

% Hold membership in a bounded political community whose welfare pool, wage floors, and self-government roster are protected by the admission regime. They fund the enforcement apparatus through taxation and carry exposure to its moral and reputational costs. Unlike the people the regime excludes, they can leave: passports confer reciprocal mobility that the regime does not restrict.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, national_citizenries, beneficiary,
    organized, biographical, mobile, national).

% Supply surveillance technology, detention facility operation, patrol services, and biometric infrastructure under government contract. Enforcement budgets are their revenue; escalation cycles are their sales pipeline. The same firms sell to multiple states, so no single regime's contraction threatens the business model.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_security_contractors, beneficiary,
    organized, immediate, arbitrage, global).

% Bear the regime's costs directly: deadly routes, debts to smugglers, detention, family separation, and deportation. There is no lawful channel available to most of them, and return is often destitution or persecution. Their exclusion is decided in forums — legislatures, courts, doctrines — they cannot reach.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Arrive claiming protection and encounter the regime at its limit-case. Where non-refoulement and similar limits are enforced they survive to process; where enforcement is externalized to transit zones or high seas interdiction, they bear refoulement risk with little recourse. Their fate tests whether the limits on admission authority bind in practice.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, refused_asylum_seekers, payer,
    powerless, biographical, trapped, regional).

% Host the enforcement that destination states push outward: processing centers, warehoused applicants, pushback corridors, and smuggling economies that destabilize local order. They receive the burdens of a regime whose decisions are made elsewhere, under agreements they did not set and cannot refuse without losing aid and diplomatic standing.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, transit_zone_communities, payer,
    moderate, generational, constrained, regional).

% Want reliable access to migrant labor and would argue for expanded legal channels, but are largely absent from the sovereignty-framed forums where admission rules are legitimated. They adapt by hiring within whatever channels exist, offshoring, or absorbing irregular-labor risk quietly.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, destination_economy_employers, excluded,
    organized, biographical, arbitrage, global).

% Monitor compliance, issue judgments and concluding observations, and develop jurisprudence that qualifies admission authority — treating the qualifying limits as inherent to legitimate authority rather than as external overlays. They command no enforcement capacity of their own and depend on domestic incorporation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, receiving_state_executives).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and polices the membership boundary of the political community: fixes who shares in self-government, the welfare pool, and labor-market protections, and gives the demos a stable roster for collective decision-making.
% TRANSFER_FUNCTION: Moves physical presence, work rights, and legal personhood from non-citizens to the citizen body; moves enforcement costs onto the migrants themselves (route mortality, smuggler debt, detention) and processing burdens onto transit states.
% ABSENT_VOICES: Excluded migrants and refused asylum seekers are the paradigmatic absent seats — the people turned back are rarely present in the legislatures, doctrines, and scholarship that legitimate the turning back. Transit-zone communities bearing the externalized enforcement, and destination-economy employers seeking labor channels, are likewise outside the sovereignty-framed conversation.
% DISAPPEARANCE_RATIONALE: If absolute exclusion discretion vanished overnight, citizenship's exclusivity, welfare-state financing assumptions, labor-market segmentation, the detention-deportation-surveillance apparatus, and the transit-state buffer arrangements would all reorganize; the enforcement industry alone represents tens of billions in annual flows that would rearrange around open or auctioned channels.
% FOUNDING_PROBLEM: After imperial dissolution and mass displacement, each polity faced the problem of defining which persons its institutions answered for: protecting nascent wages and welfare provisions from unbounded claims, consolidating national identity, and securing the demos roster that self-government presupposes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UNHCR and IOM displacement statistics attest the scale of the boundary-drawing problem; transit-state governments attest the burden-sharing problem in the diplomatic record; even the strongest critics of this reading concede the association-membership question is real while disputing its answer. No corroborating source attests that ABSOLUTE discretion specifically is required — the founding problem is live; this reading's solution to it remains contested.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, ExtMetricName, E),
    domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38 through this reading's own lights: the core exclusion function is regarded as legitimate self-determination, with the measured extractiveness attributed to implementation excesses (prolonged detention, externalized pushbacks, family separation) rather than to the authority itself. Suppression (0.62) reflects the real coercive machinery — patrols, carrier sanctions, biometric surveillance, interdiction — which the reading deems justified because the suppressed alternative (unauthorized entry) is not, on its view, a legitimate alternative at all. Theater (0.30) is rising: symbolic enforcement (wall construction, declaratory sovereignty performances) increasingly outruns functional control. Accessibility collapse is moderate (0.50) because alternative arrangements demonstrably persist — EU internal free movement, guest-worker programs, open-borders proposals — so understanding the regime does not close the possibility space. Resistance (0.62) is substantial: strategic litigation, sanctuary movements, transit-state pushback, and the smuggling economy as market-level resistance. The interval maps to roughly 1920 (passport-regime consolidation) through 2020. All three tracked series run on one shared grid {0,20,40,60,80,100}: base_extractiveness drifts mildly upward even in the reading's own assessment; theater_ratio climbs as performance grows relative to function; suppression_requirement climbs steadily as enforcement capacity hardens through externalization and biometric buildout — the story specifically tracks enforcement-capacity change, so the suppression series is authored rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the executive seat the arrangement is constitutive selfhood — the state IS its boundary authority — computing near the reading's own mountain claim. From the migrant seats the same structure operates as enforced exclusion with no lawful alternative and trapped exit, computing toward the extractive end. The citizen seat sits between: genuine benefit, diffuse tax cost, and exposure to the regime's moral externalities. The engine derives this divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: receiving_state_executives sit nearest the beneficiary pole (they capture the discretionary authority itself — the gain_flow seat), national_citizenries slightly above them (net beneficiaries who also fund enforcement and hold exit mobility), contractors low with arbitrage-grade exit. Victim declarations drive high directionality: excluded_migrants and refused_asylum_seekers approach the full-target pole, amplified by trapped exit; transit_zone_communities are elevated but moderated by their moderate power and constrained (not absent) options. The human-rights bodies occupy the analytical seat and feed no directional arithmetic. Global spatial scope on the migrant-facing side amplifies effective extraction through verification difficulty; the reading's own framing discounts this because it does not count lawful exclusion as a cost imposed.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem (defining the demos under mass displacement) is live, corroborated by displacement statistics and transit-state testimony from outside the benefiting parties. The classification discipline guards against two opposite errors. Accepting the reading's mountain claim at face value would naturalize an arrangement with named beneficiaries and victims — exactly the false-summit pattern the FSM signature exists to catch, which is why beneficiaries are declared on a mountain claim and the naturalness omega is mandatory. Conversely, flattening the arrangement to pure extraction would erase the genuine membership-coordination function that even the reading's sharpest critics concede (the association-boundary problem is real). If the engine computes tangled_rope from the structural data, that verdict preserves both halves: real coordination, real asymmetric cost-bearing, actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_border_constitutivity,
    'Is border-closure authority a genuine structural feature of statehood, as this reading claims, or a constructed arrangement maintained because identifiable agents benefit from it?',
    'Comparative-historical and institutional analysis: examine state persistence under varied admission regimes (pre-modern polities, EU internal free movement, microstates), and whether statehood functions survive decoupled from absolute exclusion discretion.',
    'If constructed, the false-summit signature fires and the constraint computes as tangled_rope (genuine coordination plus asymmetric extraction); if genuinely constitutive, mountain certification stands and the reading''s framing is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_border_constitutivity, conceptual, 'Natural-law versus constructed-constraint ambiguity for border control (FSM-required omega)').

omega_variable(
    epsilon_reading_divergence,
    'This story instantiates the sovereignty_primary reading of kernel border_control_legitimacy; the sibling readings author materially different epsilon over the same referent — how large is the divergence and what drives it?',
    'Generate the sibling stories and compare authored values: freedom_of_movement_primary is expected to author sharply higher epsilon with an expanded victim set (every denied entrant as a rights-violation); jurisdictional_sovereignty is expected to author moderate epsilon with victims limited to exclusions failing its balancing test.',
    'Large divergence confirms epsilon is reading-indexed, not topic-indexed; cross-reading comparison must hold the referent fixed and never average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_divergence, conceptual, 'Committer-frame epsilon divergence across sibling readings of the border-legitimacy kernel').

omega_variable(
    external_limits_constitutive_or_external,
    'Are the human-rights limits on exclusion (non-refoulement, extraterritorial non-return jurisprudence) genuinely external constraints on an otherwise complete authority, as this reading holds, or constitutive of what legitimate border authority IS?',
    'Doctrinal analysis of how apex courts source the limits: if they bind because admission authority is legitimate only within them, the limits are constitutive; if they bind as treaty overlays on a pre-existing complete authority, they are external.',
    'If constitutive, enforcement beyond the limits is authority exceeded rather than discretion exercised — raising effective extraction and pushing classification toward the snare side; if external, the reading''s core survives intact with the limits as bolt-on qualifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_limits_constitutive_or_external, conceptual, 'Constitutive versus external status of human-rights limits on admission authority').

omega_variable(
    suppression_deterrence_or_displacement,
    'Does intensified enforcement suppress the governed flow or displace it into deadlier routes and deeper smuggler dependence?',
    'Route-mortality and smuggling-price time series correlated against enforcement-intensity episodes (e.g., the Gatekeeper-era shift of crossings into the Sonoran desert, Mediterranean externalization effects).',
    'If displacement dominates, the measured suppression enforces the arrangement at rising human cost without shrinking the governed population — effective extraction rises even as recorded crossing counts fall, and the suppression series understates the true coercive burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_deterrence_or_displacement, empirical, 'Whether enforcement suppresses or displaces the governed migration flow').

omega_variable(
    identity_framing_cover_story,
    'Does the identity-coordination framing (''border control is who we are'') describe genuine boundary maintenance, or serve as a cover story justifying enforcement whose gains concentrate in executive authority and security contracting?',
    'Test whether the membership-boundary function persists when enforcement profits and plenary-style discretion are decoupled — e.g., statutory admission criteria administered without open-ended executive discretion, with the security market opened to competition.',
    'If the framing is cover, the identity_coordination Boltzmann leeway is being gamed and excess extraction above the coordination floor should be charged in full; if genuine, part of the measured cost is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_framing_cover_story, conceptual, 'Whether identity framing legitimizes extractive coupling in the enforcement apparatus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__sovereignty_primary, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(bord_tr_t40, observed).
narrative_ontology:measurement(bord_tr_t60, border_control_legitimacy__sovereignty_primary, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(bord_tr_t60, observed).
narrative_ontology:measurement(bord_tr_t80, border_control_legitimacy__sovereignty_primary, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(bord_tr_t80, observed).
narrative_ontology:measurement(bord_tr_t100, border_control_legitimacy__sovereignty_primary, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(bord_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__sovereignty_primary, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(bord_be_t40, observed).
narrative_ontology:measurement(bord_be_t60, border_control_legitimacy__sovereignty_primary, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(bord_be_t60, observed).
narrative_ontology:measurement(bord_be_t80, border_control_legitimacy__sovereignty_primary, base_extractiveness, 80, 0.37).
narrative_ontology:measurement_basis(bord_be_t80, observed).
narrative_ontology:measurement(bord_be_t100, border_control_legitimacy__sovereignty_primary, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(bord_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__sovereignty_primary, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(bord_su_t40, observed).
narrative_ontology:measurement(bord_su_t60, border_control_legitimacy__sovereignty_primary, suppression_requirement, 60, 0.54).
narrative_ontology:measurement_basis(bord_su_t60, observed).
narrative_ontology:measurement(bord_su_t80, border_control_legitimacy__sovereignty_primary, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(bord_su_t80, observed).
narrative_ontology:measurement(bord_su_t100, border_control_legitimacy__sovereignty_primary, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(bord_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: border_control_legitimacy decomposes into three readings over one referent (the modern exclusion-and-enforcement regime), per the epsilon-invariance principle. Epsilon differs by reading while the referent is held fixed: sovereignty_primary (this file) authors moderate-low epsilon because it reads exclusion as legitimate self-determination; freedom_of_movement_primary is expected to author high epsilon because it reads every denied entrant as a rights-violation; jurisdictional_sovereignty is expected to author moderate epsilon with victims limited to exclusions failing its balancing test. The upstream classical doctrine (this reading) supplies the legitimacy vocabulary that the downstream readings define themselves against; all three files link pairwise via affects_constraints and no values are averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
