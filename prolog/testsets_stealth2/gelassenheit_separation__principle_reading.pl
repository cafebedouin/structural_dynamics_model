% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation — Structural Entanglement Criterion (Principle Reading)
 *   domain: religious/social/technological
 *
 * SUMMARY:
 *   The Old Order Amish and kindred conservative Anabaptist separation
 *   arrangement, read through its structural-entanglement criterion (the
 *   principle reading of the gelassenheit_separation kernel): the
 *   congregation refuses entanglement in outside administrative systems —
 *   commercial insurance, public-grid electricity, home internet, state
 *   welfare — so that risk-pooling, dispute resolution, economic standards,
 *   and obligation run through the congregation rather than through worldly
 *   institutions. Technology is admitted when it can be adopted while
 *   functionally isolated from those systems (off-grid solar, pneumatic and
 *   hydraulic tool trains, diesel-generated power, propane refrigeration) and
 *   refused when adoption would transfer obligation or dependence outward,
 *   regardless of whether isolation could be arranged (home internet,
 *   commercial insurance, grid connection). The epsilon referent is the
 *   standing separation arrangement as this reading assesses it — never the
 *   arrangement a sibling reading would endorse. This story is one member of
 *   a three-story constraint family decomposed from the single kernel per the
 *   epsilon-invariance principle: the artifact reading and the consequence
 *   reading are separate constraints with their own epsilon and victim sets,
 *   linked through network.affects_constraints. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED here as tangled_rope — genuine
 *   coordination carrying real asymmetric costs — while the metrics are
 *   authored independently from its observed operation; the engine computes
 *   per-seat classifications from the structural data. KEY AGENTS (by
 *   structural relationship): - ordo_keeping_ministry: Agenda-setter
 *   (organized/identity_locked) — administers the Ordnung, adjudicates
 *   technology cases, imposes shunning; lives under the same rules it
 *   enforces - baptized_community_members: Primary beneficiary
 *   (moderate/identity_locked) — receives mutual aid, communal governance,
 *   cultural continuity; pays in foregone options and discipline exposure -
 *   dissenting_members: Primary target (powerless/trapped) — bears the
 *   arrangement where preferences diverge; exit costs are family, community,
 *   livelihood - rumspringa_youth: Target-before-commitment
 *   (powerless/constrained) — preferences count only until baptism; family
 *   and formation point toward joining - shunned_former_members:
 *   Target-after-exit (powerless/trapped) — the boundary follows them into
 *   family relations after they leave - commercial_insurers_and_carriers:
 *   Excluded party (institutional/mobile) — would sell insurance, power,
 *   connectivity; the refusal is the arrangement's point -
 *   outside_courts_and_regulators: Analytical observer
 *   (institutional/analytical) — adjudicate where refusal meets state
 *   systems; set the boundary's outer edge
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.52).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.58).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation — Structural Entanglement Criterion (Principle Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/social/technological").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '98c901d6-7c31-435d-a8f3-82979aeb09ca').
narrative_ontology:cs_kernel_codification('98c901d6-7c31-435d-a8f3-82979aeb09ca', distributed).
narrative_ontology:cs_authority_grounding('98c901d6-7c31-435d-a8f3-82979aeb09ca', practice).
narrative_ontology:cs_interpretation_layer_present('98c901d6-7c31-435d-a8f3-82979aeb09ca').
narrative_ontology:cs_reading_relation('98c901d6-7c31-435d-a8f3-82979aeb09ca', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('98c901d6-7c31-435d-a8f3-82979aeb09ca', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('98c901d6-7c31-435d-a8f3-82979aeb09ca', foundational, separation_criterion_is_structural_entanglement).
narrative_ontology:cs_axiom_status(separation_criterion_is_structural_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('98c901d6-7c31-435d-a8f3-82979aeb09ca', separation_criterion_is_structural_entanglement, theological).
narrative_ontology:cs_axiom('98c901d6-7c31-435d-a8f3-82979aeb09ca', secondary, functional_isolation_test_for_technology).
narrative_ontology:cs_axiom_status(functional_isolation_test_for_technology, holdable).
narrative_ontology:cs_axiom_grounding('98c901d6-7c31-435d-a8f3-82979aeb09ca', functional_isolation_test_for_technology, instrumental).
narrative_ontology:cs_reference_frame('98c901d6-7c31-435d-a8f3-82979aeb09ca', congregation_administered_life).
narrative_ontology:cs_drift_state('98c901d6-7c31-435d-a8f3-82979aeb09ca', contemporary_offgrid_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('98c901d6-7c31-435d-a8f3-82979aeb09ca', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, baptized_community_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordo_keeping_ministry).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, dissenting_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, rumspringa_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, shunned_former_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, baptized_community_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, gelassenheit_yieldedness_ideal).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, congregational_self_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and ministers, chosen by lot from within the congregation, administer each district's Ordnung: they decide which technologies members may adopt, hear confessions, impose shunning (Meidung) for defiance, and represent the district in settlement-wide conferences. They live under the same rules they administer, serve without salary, and their standing exists only inside the community; leaving would cost them family, congregation, and the identity they have held since baptism.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordo_keeping_ministry, agenda_setter,
    organized, generational, identity_locked, regional).

% Adult members who joined by choice at baptism. They receive what the arrangement protects: congregation-run mutual aid in place of insurance, communal dispute resolution, a stable economic niche in farming and trades, and a way of life they can hand to their children. They pay in foregone options — no grid power, no commercial insurance, no home internet — and their technology choices are subject to district adjudication. Leaving after baptism triggers shunning and family severance.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, baptized_community_members, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, baptized_community_members, payer).

% Members whose preferences or business needs diverge from the district's rules — a craftsman who wants an online storefront, a farmer who wants a cell phone for orders, a parent who wants a health policy for a sick child. They can argue their case before the district, comply, or defy and face Meidung. Leaving costs extended family, congregation, and a trade learned in a community that ends schooling at the eighth grade.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, dissenting_members, payer,
    powerless, biographical, trapped, regional).

% Adolescents in the running-around years who face the baptism decision. Until baptism they may sample the outside world without shunning; baptism commits them to the Ordnung for life. Their preferences count only before commitment, and family, schooling, and work formation all point toward joining. Those who decline lose daily community life, though most districts do not shun the never-baptized.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, rumspringa_youth, payer,
    powerless, biographical, constrained, regional).

% Former members who left or were expelled. In strict districts the Meidung follows them out: baptized relatives may not eat at their table, do business with them, or ride in their vehicles. The boundary they left continues to govern their closest relationships from the outside; only rejoining, or a district relaxing the discipline, changes it.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, shunned_former_members, payer,
    powerless, biographical, trapped, regional).

% Insurers, utilities, telecom carriers, and lenders who would sell risk coverage, grid power, connectivity, and credit to the community. There is no negotiation to be part of: the community's refusal to purchase is the arrangement working as designed. They lose a compact, distinctive market but hold abundant alternatives elsewhere.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, commercial_insurers_and_carriers, excluded,
    institutional, generational, mobile, national).

% Courts and agencies that adjudicate where the community's refusals meet state systems: compulsory schooling past the eighth grade, Social Security participation, workplace and building codes, road-safety requirements. Wisconsin v. Yoder and the Social Security conflicts set precedents for which refusals the state tolerates. They shape the boundary's outer edge without holding any seat in the Ordnung itself.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, outside_courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem for a dispersed, economically integrated religious minority: how to keep risk-pooling (mutual aid in place of insurance), dispute resolution, education, and technological standards under congregational administration rather than ceding them to outside systems whose participation terms — premiums, grid contracts, platform dependence, state administration — would progressively reconstitute members' obligations toward the world and dissolve the congregation's self-governance. The boundary is the mechanism: refusing entanglement keeps the coordination problems inside the community, where they are solved by communal labor and lot-chosen ministry.
% TRANSFER_FUNCTION: Moves obligation and risk from outside systems into the congregation: premium-equivalent labor and wealth flow through mutual aid from members in good times to members in catastrophe instead of to commercial carriers; technological and economic choices flow from individual preference to district adjudication; and the costs of the boundary itself — foregone options, discipline, post-exit severance — fall on dissenting members, pre-commitment youth, and leavers, whose compliance sustains the boundary that benefits the member body as a whole.
% ABSENT_VOICES: The structurally silenced voice is the shunned former member: Meidung is designed so the community does not hear leavers — their objection is not outvoted but made unspeakable inside the district. Rumspringa youth are present but their preferences carry weight only until baptism. Commercial carriers, utilities, and telecoms have no seat in the Ordnung process at all; their exclusion is the arrangement's enforcement object. Dissenting members are heard through adjudication but outvoted and bound by district assent.
% DISAPPEARANCE_RATIONALE: If the separation arrangement vanished overnight, members would connect to the grid, buy insurance, adopt home internet, and shift risk-pooling and dispute resolution to commercial and state systems; the mutual aid network would lose its pool; the ministry would lose its adjudicative function; and within a generation or two the community would assimilate along the path of more assimilated Anabaptist groups that relaxed separation. The arrangement is load-bearing for the community's persistence as a distinct people — its disappearance rearranges the world of every seat in the story.
% FOUNDING_PROBLEM: The 16th-century Anabaptist founding problem, carried through the 1693 Amman division and the 19th-century Old Order hardening: how a voluntarily gathered church can remain a disciplined, self-administering peculiar people — with its own discipline, mutual aid, and way of life — inside state churches and, later, American mass society and industrial modernity, when every outside system it touches arrives with participation terms that reconstitute members' obligations toward the world.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic Anabaptist historiography (the Hostetler and Kraybill ethnographies among others) documents the founding problem and its continuing re-presentation by each technology wave; court records from the schooling and Social Security conflicts, through Wisconsin v. Yoder, attest both the community's refusals and the state systems pressing on them; former-member memoirs attest the costs from the payer side. No source outside the beneficiary set attests that the structural-entanglement criterion — as opposed to the artifact or consequence criteria — is the kernel's correct sense; that contest is internal to the tradition and remains open.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52 is moderate: most governed parties are consenting net beneficiaries who joined as adults, but the arrangement takes real costs from parties whose consent is absent or expired — dissenters under discipline, youth whose preferences lapse at baptism, leavers under Meidung. It sits below the artifact reading's expected extractiveness because the entanglement criterion's carve-out logic (functional isolation) admits genuinely useful technology and shrinks the surface of arbitrary refusal. Suppression 0.58 is authored as a raw structural property, unscaled by power or scope: shunning, schooling terminated at the eighth grade, and economic formation inside the community; it is moderated by adult baptism, by pre-baptism exit without shunning, and by the Ordnung's revisability through district assent. Theater_ratio 0.16: the functions are real and load-bearing — mutual aid pays claims, the ministry adjudicates, the boundary holds; visible markers carry some performative weight, but that is the artifact reading's center of gravity, not this one's. Accessibility_collapse 0.35: alternatives do not collapse — within the permitted set, substitutes exist for most refused systems (mutual aid for insurance, off-grid generation for the grid, shop-located telephony for home internet) and the outside remains reachable at the price of exit. Resistance 0.45: a century of district splits, dissent, and litigation (schooling conflicts through Wisconsin v. Yoder, Social Security refusal) shows real but bounded resistance; most of it is absorbed by adjudication or by exit into new districts — which is how the powerless seats' coalition power expresses itself here, as fission rather than internal reform. The three measurement series share one grid (1920, 1940, 1960, 1980, 2000, 2020; every tracked metric at every point). The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the discipline machinery hardened from 1920 to 1960 — cars, grid power, and home telephones refused, Meidung applied to holdouts — then stabilized as the carve-out logic matured, while base_extractiveness drifts gently down across the same span as permitted-technology latitude widens.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the ministry seat the arrangement is covenant machinery administered at personal cost, with no rent collected and no exit; from the baptized-member seat it is a net-benefit covenant with real foregone options; from the dissenter, youth, and leaver seats the same rules operate as enforced extraction with ruinous or severed exits. The divergence is driven by directionality and exit options rather than raw power: the ministry and devout members are identity-locked into benefit — their religious self-concept is constituted by the yieldedness the Ordnung codifies, so exit is not merely costly but unthinkable within the identity frame — while dissenters are trapped into payment by family, formation, and livelihood. If the identity frame broke, as it does for leavers, the same structural rules would present as pure external barriers and the payer-seat reading would dominate the computation. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: baptized_community_members (mutual aid, governance, continuity) and ordo_keeping_ministry (role continuity, adjudicative standing) derive low directionality — the arrangement subsidizes them, though member benefit is reciprocal and diffuse rather than captured. Victim declarations: dissenting_members, rumspringa_youth, and shunned_former_members derive high directionality — they bear compliance costs, expired preferences, and post-exit severance respectively, with trapped or constrained exit pushing them toward the full-target end. Commercial carriers are excluded rather than coordinated — the arrangement's enforcement object is their non-admission. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the right directionality for every seat. The one candidate override — the ministry, whose authority gain might warrant adjusting a derived near-beneficiary value upward — was rejected because that gain is burden-coupled and non-rental: ministers are chosen by lot, serve unpaid, and live under identical rules with no concentrated receipt.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two mislabels. Reading the arrangement as a snare would erase the genuine coordination function: mutual aid demonstrably substitutes for insurance, the congregation demonstrably self-governs, and the majority joined as consenting adults — the asymmetric costs are real but ride on a working coordination structure, which is the definition of the hybrid. Reading it as a rope would erase the costs borne by parties who never consented or whose consent expired: dissenters under discipline, youth whose preferences count only until baptism, leavers under Meidung. Mandatrophy is not in play as decay: the founding problem — preserving communal self-administration against entangling systems — is live, re-presented by each new technology wave, and the arrangement's functions are un-atrophied (theater 0.16). This is not a piton maintained by inertia, and it is not a scaffold: there is no sunset, because the arrangement's justification is the steady state, not a transition. The prohibitive fixing_cost reflects constitutive commitment — removal would dissolve the beneficiary community itself — not atrophic inertia; the prohibitive-plus-diffuse receipt cell should not be misread as the piton signature, since the diffuse finding is an affirmative checked claim (no seat captures: the ministry's gain is unpaid and burden-coupled, member gains are reciprocal within the seat) and the cost asymmetry runs the opposite way from the piton test — the administrators could not fix it without destroying what they are.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_criterion_underdetermination,
    'Does the gelassenheit_separation kernel itself determine which criterion governs technology admission — visible artifact distinction, structural-entanglement avoidance, or community-practice consequence — or is the kernel under-determined such that this story''s epsilon holds only under the principle criterion?',
    'Observe how districts adjudicate novel technologies on which the criteria diverge — e.g., off-grid solar that visibly resembles worldly installations, or a low-entanglement device with high community-effect risk such as a shared shop computer. Divergent district verdicts on identical artifacts would confirm under-determination; convergence on the entanglement test across districts would confirm this reading as the kernel''s operative sense.',
    'If the artifact criterion governs, extractiveness rises (more forbidden by appearance, more arbitrary refusals); if the consequence criterion governs, the victim set shifts toward members whose usage erodes visiting and mutual aid. This story''s extractiveness, beneficiary structure, and victim set are authored under the principle criterion and would need re-authoring under either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_criterion_underdetermination, conceptual, 'Kernel under-determination across the three readings of gelassenheit_separation; this story instantiates the principle reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissenting members structural (shunning, exit costs, schooling terminated at the eighth grade, economic formation inside the community) or internalized (identity fusion with Gelassenheit such that dissent is unthinkable before any barrier is met)?',
    'Post-exit trajectory of leavers: if leavers report the arrangement''s pull persisting after all structural barriers are removed, and rejoin at high rates, a large internalized component is established; if leavers who escape the structural barriers integrate readily, suppression is mostly structural.',
    'If largely internalized, effective suppression exceeds the structural measure and the payer seats compute closer to a snare profile; if largely structural, the tangled_rope reading with moderate suppression holds, since the barriers are visible, nameable, and partly negotiable through the Ordnung process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether community discipline suppresses through external barriers, identity fusion, or both.').

omega_variable(
    internet_isolation_boundary_stress,
    'Can the regardless-of-isolation prohibition on internet hold as members'' trades increasingly require e-commerce and digital customer contact, or will districts carve out mediated or isolated internet uses?',
    'Track district Ordnung adjudications on internet-adjacent cases over the coming decade: mediated access through non-member intermediaries, shared business machines located in shops rather than homes, delegated web presence run by outside agents.',
    'Carve-outs would extend the principle reading''s permissive trajectory (extractiveness drifting further down) while raising the question whether mediated access is genuinely isolation; a hard line would raise the costs borne by business-dependent members and test whether payer-seat resistance grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_isolation_boundary_stress, empirical, 'Stress test of the principle reading''s internet prohibition at the boundary of trade necessity.').

omega_variable(
    catastrophic_risk_externalization,
    'Does refusal of commercial insurance, combined with congregation-run mutual aid, leave catastrophic and long-tail risks (nursing care, lifelong disability) externalized onto public systems the community does not fund into — such that part of the arrangement''s low internal extractiveness is financed by an unmeasured transfer from outside taxpayers?',
    'Audit mutual-aid coverage against actuarial risk classes; compare members'' utilization of public assistance programs (e.g., Medicaid long-term care) against premium-equivalent community contributions.',
    'If externalization is substantial, internal metrics understate the arrangement''s effective extraction — part of it falls on parties who hold no seat in this story — which would raise effective extraction and complicate the beneficiary accounting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophic_risk_externalization, empirical, 'Whether mutual aid fully substitutes for insurance or externalizes tail risks onto outside systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t1920, gelassenheit_separation__principle_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(gela_tr_t1920, observed).
narrative_ontology:measurement(gela_tr_t1940, gelassenheit_separation__principle_reading, theater_ratio, 1940, 0.14).
narrative_ontology:measurement_basis(gela_tr_t1940, observed).
narrative_ontology:measurement(gela_tr_t1960, gelassenheit_separation__principle_reading, theater_ratio, 1960, 0.14).
narrative_ontology:measurement_basis(gela_tr_t1960, observed).
narrative_ontology:measurement(gela_tr_t1980, gelassenheit_separation__principle_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(gela_tr_t1980, observed).
narrative_ontology:measurement(gela_tr_t2000, gelassenheit_separation__principle_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(gela_tr_t2000, observed).
narrative_ontology:measurement(gela_tr_t2020, gelassenheit_separation__principle_reading, theater_ratio, 2020, 0.16).
narrative_ontology:measurement_basis(gela_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(gela_be_t1920, gelassenheit_separation__principle_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement_basis(gela_be_t1920, observed).
narrative_ontology:measurement(gela_be_t1940, gelassenheit_separation__principle_reading, base_extractiveness, 1940, 0.58).
narrative_ontology:measurement_basis(gela_be_t1940, observed).
narrative_ontology:measurement(gela_be_t1960, gelassenheit_separation__principle_reading, base_extractiveness, 1960, 0.57).
narrative_ontology:measurement_basis(gela_be_t1960, observed).
narrative_ontology:measurement(gela_be_t1980, gelassenheit_separation__principle_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement_basis(gela_be_t1980, observed).
narrative_ontology:measurement(gela_be_t2000, gelassenheit_separation__principle_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(gela_be_t2000, observed).
narrative_ontology:measurement(gela_be_t2020, gelassenheit_separation__principle_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement_basis(gela_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t1920, gelassenheit_separation__principle_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement_basis(gela_su_t1920, observed).
narrative_ontology:measurement(gela_su_t1940, gelassenheit_separation__principle_reading, suppression_requirement, 1940, 0.58).
narrative_ontology:measurement_basis(gela_su_t1940, observed).
narrative_ontology:measurement(gela_su_t1960, gelassenheit_separation__principle_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement_basis(gela_su_t1960, observed).
narrative_ontology:measurement(gela_su_t1980, gelassenheit_separation__principle_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(gela_su_t1980, observed).
narrative_ontology:measurement(gela_su_t2000, gelassenheit_separation__principle_reading, suppression_requirement, 2000, 0.59).
narrative_ontology:measurement_basis(gela_su_t2000, observed).
narrative_ontology:measurement(gela_su_t2020, gelassenheit_separation__principle_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement_basis(gela_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% 'Gelassenheit separation' is a single persisting commitment read three ways; each reading instantiates a different constraint with its own epsilon and victim set, decomposed per the epsilon-invariance principle. This (principle) story authors epsilon for the standing separation arrangement assessed by the structural-entanglement criterion — moderate, because the criterion's carve-out logic (functional isolation) widens permitted technology and shrinks the arbitrary-refusal surface. The artifact reading (visible distinction from English society) authors a higher-epsilon constraint: appearance-based prohibition refuses functionally isolated tools this reading admits. The consequence reading (preservation of visiting, mutual aid, rootedness) authors a different victim set and a different epsilon. No strict upstream/downstream ordering holds among the three, but this reading's carve-out logic changes the legitimacy conditions under which the siblings' prohibitions are defended: districts that admit solar under a functional-isolation test face pressure to justify artifact-based and consequence-based refusals in kind.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
