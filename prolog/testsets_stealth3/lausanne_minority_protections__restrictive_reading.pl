% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Restrictive Reading of the Lausanne Minority Protections: Individual Worship Only, Institutional Matters Reserved to Domestic Jurisdiction
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   Under this reading, the 1923 Treaty of Lausanne's minority chapter
 *   guarantees individual religious observance and nothing more: communal
 *   property belongs to the general foundations regime, minority schools
 *   answer to the education ministry, theological seminaries fall under
 *   private-education restrictions, and communal institutions hold no
 *   independent legal personality. The reading is the operative interpretive
 *   frame in Turkish practice, and it does measurable work: it is the premise
 *   on which foundation properties entered state stewardship, on which the
 *   Halki seminary stayed closed, and on which guarantor-state objection is
 *   classified as interference. This file authors ONLY the restrictive
 *   reading as one clean, epsilon-invariant constraint (Rule 1 of the
 *   committer frame); the expansive and guarantor readings are separate
 *   constraints in separate files, linked through the network section, with
 *   their committer content routed to omega variables. KEY AGENTS (by
 *   structural relationship): - turkish_state_apparatus: agenda setter and
 *   principal beneficiary (institutional/arbitrage) - interprets the kernel,
 *   administers all communal matters under domestic law, collects the
 *   discretion and asset-control gains - directorate_general_of_foundations:
 *   administering beneficiary (institutional/mobile) - stewards sequestered
 *   vakif assets and collects endowment income -
 *   greek_orthodox_community_institutions: primary payer
 *   (moderate/identity_locked) - Patriarchate, vakifs, schools; property
 *   forfeiture, seminary closure, legal-personality denial -
 *   armenian_apostolic_community_institutions: payer
 *   (moderate/identity_locked) - patriarchate and foundation network under
 *   the same regime, thinner international leverage -
 *   jewish_community_institutions: payer (organized/identity_locked) -
 *   smallest portfolio, least external attention, shortest planning horizon -
 *   lay_members_of_minority_communities: dual-positioned
 *   (powerless/constrained) - keep individual worship, carry service losses
 *   and diaspora subsidy burdens - hellenic_government: excluded voice
 *   (institutional/constrained) - protective claims recast as interference in
 *   domestic affairs - european_human_rights_machinery: analytical observer
 *   (institutional/analytical) - adjudicates petitions, pressures reform
 *   without overturning the frame
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.74).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.78).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive Reading of the Lausanne Minority Protections: Individual Worship Only, Institutional Matters Reserved to Domestic Jurisdiction").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'f16b056e-02cc-452c-95ab-97a087260f1d').
narrative_ontology:cs_kernel_codification('f16b056e-02cc-452c-95ab-97a087260f1d', fixed_text).
narrative_ontology:cs_authority_grounding('f16b056e-02cc-452c-95ab-97a087260f1d', extraction).
narrative_ontology:cs_interpretation_layer_present('f16b056e-02cc-452c-95ab-97a087260f1d').
narrative_ontology:cs_reading_relation('f16b056e-02cc-452c-95ab-97a087260f1d', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('f16b056e-02cc-452c-95ab-97a087260f1d', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('f16b056e-02cc-452c-95ab-97a087260f1d', foundational, lausanne_scope_limited_to_individual_worship).
narrative_ontology:cs_axiom_status(lausanne_scope_limited_to_individual_worship, holdable).
narrative_ontology:cs_axiom_grounding('f16b056e-02cc-452c-95ab-97a087260f1d', lausanne_scope_limited_to_individual_worship, conventional).
narrative_ontology:cs_axiom('f16b056e-02cc-452c-95ab-97a087260f1d', secondary, communal_questions_reserved_to_domestic_jurisdiction).
narrative_ontology:cs_axiom_status(communal_questions_reserved_to_domestic_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('f16b056e-02cc-452c-95ab-97a087260f1d', communal_questions_reserved_to_domestic_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('f16b056e-02cc-452c-95ab-97a087260f1d', individual_worship_domestic_jurisdiction_frame).
narrative_ontology:cs_drift_state('f16b056e-02cc-452c-95ab-97a087260f1d', post_partial_restitution_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f16b056e-02cc-452c-95ab-97a087260f1d', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, greek_orthodox_community_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_apostolic_community_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, jewish_community_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, lay_members_of_minority_communities).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, lay_members_of_minority_communities).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, absolute_domestic_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__restrictive_reading, unitary_nation_homogenization_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the 1923 treaty's minority chapter as guaranteeing individual religious observance only, and administers every communal question - foundation property, minority schooling, clerical training, institutional legal status - through ordinary domestic legislation and courts. Holds the discretion to sequester, restrict, or dissolve communal institutions, and cites the treaty's domestic-jurisdiction character against outside objection.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers minority pious foundations whose governance or property has passed under state stewardship, collecting rental and endowment income and deciding maintenance, sales, and transfers. Operates under the foundations statute and answers to the general state budget process.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations, agenda_setter).

% Maintain the Patriarchate, dozens of pious foundations, and minority schools in Istanbul and on the islands. A 1936 property-declaration requirement, combined with later court rulings voiding post-1936 acquisitions, removed most foundation real estate; the theological seminary on Heybeliada has been closed since 1971 under private-education rules, ending local clergy formation; the state treats the Patriarchate as a local church office without legal personality. Members continue to worship freely; institutional capacity narrows yearly through attrition, emigration, and denied registrations.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, greek_orthodox_community_institutions, payer,
    moderate, civilizational, identity_locked, continental).

% Operate a patriarchate, a network of foundations, schools, and a community press under the same property and education regime. Community numbers fell sharply after mid-century shocks, thinning the base that sustains the institutions; board elections and property claims proceed through the same state-supervised channels.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_apostolic_community_institutions, payer,
    moderate, generational, identity_locked, national).

% Run a small chief rabbinate, a handful of foundations, and one secondary school under the same regime, with the smallest property portfolio and the thinnest international attention of the three communities; demographic decline drives short-horizon institutional planning.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, jewish_community_institutions, payer,
    organized, biographical, identity_locked, national).

% Attend worship and community services that remain guaranteed, send children to minority schools where available, and finance institutional gaps through donations and diaspora remittances. Emigration is possible and common, but those who stay are bound to the institutions that anchor communal life, and those who leave rarely retain standing in foundation governance.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, lay_members_of_minority_communities, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, lay_members_of_minority_communities, payer).

% Asserts a recognized protective interest in the Patriarchate and the Greek minority under the treaty's guarantor architecture. Its demarches and protests are received as interference in domestic affairs, since the interpretation in force classifies the underlying matters as internal; Athens retains bilateral and European channels but no standing inside the interpretive framework that settles these questions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, hellenic_government, excluded,
    institutional, generational, constrained, continental).

% Receives petitions from foundations and the Patriarchate, has ruled in several landmark cases (notably returning the Buyukada orphanage to the Patriarchate) that property deprivations lacked justification under convention standards, and monitors reform commitments through committee procedures. Its judgments pressure legislative change but do not themselves revise the interpretive frame.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_human_rights_machinery, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, directorate_general_of_foundations).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the jurisdictional ambiguity of the treaty's minority chapter by allocating all communal questions to domestic law, giving the state and its courts a single rule for administering minority foundations, schools, and clergy formation, while individual worship remains guaranteed and uncontested.
% TRANSFER_FUNCTION: Moves real property, endowment income, governance authority, and clerical and educational human-capital formation from Greek Orthodox, Armenian Apostolic, and Jewish communal institutions to the Turkish state's administrative apparatus (Directorate General of Foundations, Treasury, education ministry) via forfeiture rulings, closure decisions, and denial of legal personality.
% ABSENT_VOICES: Guarantor states and European supervisory mechanisms would object to the domestic-exclusivity allocation but are structurally outside it: the reading's core move is classifying institutional matters as internal, recasting their objections as interference. The minority communities themselves had no negotiating presence at Lausanne and hold no seat in the interpretive tradition that fixed this reading.
% DISAPPEARANCE_RATIONALE: If the restrictive allocation vanished overnight - protections suddenly covering institutional autonomy, property, and theological education - minority foundations would re-register legal personalities, mass restitution claims would open, the Halki seminary would move to reopen, minority school governance would restructure away from ministry-appointed boards, and diplomatic relations would realign around guarantor-state enforcement. Both the state's administrative machinery and the communities' institutional life reorganize around the changed rule.
% FOUNDING_PROBLEM: Post-imperial sovereignty consolidation: after 1923 the republic needed to secure that minority communal networks - churches, schools, foundations with dense cross-border ties - could not serve as instruments of foreign influence or irredentism, while conceding enough of the treaty to settle great-power recognition. The restrictive reading was built to square that circle: guarantee what individuals need in order to worship, reserve whatever institutions could leverage.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: European Court of Human Rights judgments in foundation-property cases treat the deprivations as unjustified under contemporary standards; EU accession reports and UN minority-rights reviews repeatedly characterize the institutional exclusions as policy choices rather than security necessities; diplomatic historiography documents the reading's consolidation across the 1936-1974 sequence as administrative strategy. Turkish official positions attest the problem remains live, citing national unity and kin-state interference fears. No disinterested source attests the founding problem as dead, and no source inside the beneficiary set treats it as solved and merely inertia-maintained.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the reading's operation moves titled property, endowment income, governance authority, and clergy-formation capacity from three named communities to the state's administrative apparatus, with the 1974 Court of Cassation forfeiture doctrine converting the 1936 declaration requirement into a standing confiscation channel. Suppression is high (0.78) because persistence depends on active machinery - foundation administration, education-law gates, legal-personality denial - not on participant preference; suppression is authored as a raw structural property and is NOT scaled by power or scope (only extractiveness scales through directionality and scope in the engine's computation). Theater is moderate-low (0.32): the individual-worship guarantee is genuinely delivered, but a growing share of activity is compliance performance - treaty citations in international reporting that present protection as adequate while institutional substance erodes. Accessibility collapse is 0.58: once the reading is understood, alternatives narrow sharply (no independent legal vehicle exists outside state-supervised forms, and international recourse is framed out), but diaspora resources, Strasbourg petitions, and periodic reform windows keep partial alternatives alive. Resistance is 0.62: sustained litigation with several Court of Human Rights wins, annual diplomatic demarches, and report pressure - substantial but bounded, never frame-breaking. The temporal series run on one shared grid (1924/1942/1964/1974/1990/2008/2024). The arc is a ratchet, not a cycle: enforcement builds through the 1936 declarations, the mid-century expulsion wave, and the 1974 judicial ratchet (suppression peaking 0.84), then partially decays in the EU-accession reform window (2008-2011 restitution statutes), then re-hardens as promised further reforms stall (0.78 by 2024). The 2008 dip is reform-driven, not oscillatory; no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the state's seat the arrangement presents as settled jurisdictional coordination it built and maintains - a rope-shaped experience: ambiguity resolved, worship guaranteed, communal questions handled by ordinary law. From the three payer seats the same structure operates as enforced dispossession with no exit: identity-lock binds each institution to its communal selfhood, so leaving the framework means dissolving the thing itself (professional-relational-institutional fusion: the Patriarchate cannot relocate without ceasing to be what it is; a foundation board cannot incorporate elsewhere because permitted corporate forms are themselves state-defined). The excluded guarantor seat experiences bad-faith confinement - a treaty obligation invoked selectively. The analytical seat sees adjudicable grievances inside an unmovable interpretive frame. Coalition check: the three payer communities plus lay members share a nominal position but are administered separately and differ in leverage (Greek institutions hold the largest property stake and the Patriarchate's standing; Armenian institutions hold mid-scale assets and few external levers; Jewish institutions hold the smallest portfolio and shortest horizon), and demographic attrition continuously thins any coalition base - collective action is possible (joint petitions occur) but structurally discouraged.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared beneficiaries sit at the low-directionality end: the state apparatus receives the discretionary control the frame preserves, and the foundations directorate receives administered assets and endowment income. The three declared victims sit near the full-target end, amplified by identity_locked exit - a community that cannot leave absorbs the full effective load, unlike a mobile actor who could arbitrage away. Lay members are genuinely dual: they collect the individual-worship guarantee (pulling d toward the beneficiary end) while bearing institutional-service losses and remittance burdens (pushing toward target), landing near symmetric. The excluded and observer seats take no flow; they derive from fallback rather than from beneficiary or victim declarations. Scope is national, which modestly amplifies effective extraction through harder verification, but no directionality overrides are needed: the beneficiary and victim declarations already produce the correct d ordering for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is refusing the coordination cover. The arrangement advertises jurisdictional clarity and reciprocity fairness (the Greek minority of Western Thrace is the standing comparison) - a story under which it would read as a rope. Declaring the beneficiaries, victims, and enforcement requirement forces the engine to price the asymmetry instead of accepting the cover. Mandatrophy is deliberately NOT resolved: founding_problem_status is authored 'contested', not 'dead', because the security concern the reading was built for (kin-state leverage through communal networks) has genuine adherents and genuine skeptics, and declaring it dead would fabricate consensus. If later evidence resolves the founding problem as dead while disappearance_verdict stays world_rearranges, the status-x-verdict mismatch fires the capture/zombie flag, which cross-checks against the theater path - currently 0.32, well below piton-theater territory; this is a functioning extraction, not a theatrical husk. base_properties.mandatrophy_resolved is accordingly left unset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_kernel_reading_commitment,
    'Which reading correctly fixes the scope of the Lausanne minority chapter, and does this story''s classification survive if a sibling reading prevails?',
    'Comparative classification across the three sibling stories plus drafting-history analysis (Lausanne Conference records and the travaux on Articles 39-41); the sibling files carry their own epsilon and victim sets.',
    'If the expansive reading prevails, minority institutions exit the victim set, the state''s beneficiary position converts into that of an administrator of a genuine coordination arrangement, and this story''s snare classification inverts; if the guarantor reading prevails, adjudication relocates outward and the domestic-enforcement dependency that sustains this constraint weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_kernel_reading_commitment, conceptual, 'Committer-frame routing: this story is one reading of a contested kernel; sibling readings are separate constraints, not parts of this one.').

omega_variable(
    scope_axis_disagreement_location,
    'Is the readings'' dispute located solely on the scope axis (individual versus communal coverage), with all siblings agreeing that individual worship is protected?',
    'Parse each sibling reading''s authored axioms and confirm that the restrictive-expansive opposition runs exclusively on the coverage-scope allocation while the guarantor opposition runs on adjudication locus - two distinct axes.',
    'Confirms why the restrictive-to-expansive relation is foreclosure (exclusive-or scope allocation within any single framework) while the restrictive-to-guarantor relation is not (a framework can hold narrow scope plus external supervision); miscategorizing the axes would corrupt the foreclosure computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_axis_disagreement_location, conceptual, 'Locates the structural element on which sibling readings actually differ: coverage scope versus enforcement locus.').

omega_variable(
    restitution_absorption_or_convergence,
    'Do the 2008-2011 restitution statutes represent genuine convergence toward the expansive allocation, or absorptive reinterpretation that concedes individual cases while preserving the restrictive frame?',
    'Track pending foundation applications, Halki reopening status, incidence of new confiscation events after 2011, and whether returned properties carry restored governance rights or merely custodial title.',
    'Convergence would push this story down a tangled_rope drift path (mutual-benefit coordination emerging inside the old frame); absorption leaves the snare profile intact with the frame''s authority absorbing drift through its interpretive layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_absorption_or_convergence, empirical, 'Whether the reform era is frame revision or frame-preserving concession.').

omega_variable(
    reciprocity_symmetry_premise,
    'Does the reciprocity argument - comparison with the treatment of the Muslim minority of Western Thrace - factually support equivalent restriction, as the restrictive reading''s fairness cover assumes?',
    'Comparative audit of the Thrace muftiate, vakif, and minority-school regimes against the Turkish minority regime, scored on the same dimensions (legal personality, property control, clergy formation, school governance).',
    'Factual symmetry would strengthen the reading''s fairness cover and raise the plausibility of its coordination framing; asymmetry exposes the cover as rationalization, sharpening the extraction diagnosis and weakening the reciprocity defense in international fora.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_symmetry_premise, empirical, 'Whether the reciprocity premise is empirically true or a selective comparison.').

omega_variable(
    internalized_claim_reticence,
    'How much of the observed resistance level reflects internalized reticence learned through the mid-century shocks (wealth-tax expropriation, expulsion wave, pogrom aftermath) rather than genuine acquiescence in the arrangement?',
    'Compare claim-lodging rates before and after the Court of Human Rights victories, and resident-community litigation rates against diaspora-sponsored litigation rates on identical grievance classes.',
    'If reticence is largely internalized, effective suppression exceeds the structural 0.78 and authored resistance understates latent opposition; classification is unchanged but enforcement-cost estimates fall and reform-pressure forecasts rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_claim_reticence, empirical, 'Structural versus internalized component of the measured suppression and resistance levels; the structural component dominates (roughly seven-tenths legal-barrier, three-tenths learned-reticence by the available behavioral evidence), but the split is uncertain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 1924, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1924, lausanne_minority_protections__restrictive_reading, theater_ratio, 1924, 0.12).
narrative_ontology:measurement_basis(laus_tr_t1924, observed).
narrative_ontology:measurement(laus_tr_t1942, lausanne_minority_protections__restrictive_reading, theater_ratio, 1942, 0.16).
narrative_ontology:measurement_basis(laus_tr_t1942, observed).
narrative_ontology:measurement(laus_tr_t1964, lausanne_minority_protections__restrictive_reading, theater_ratio, 1964, 0.22).
narrative_ontology:measurement_basis(laus_tr_t1964, observed).
narrative_ontology:measurement(laus_tr_t1974, lausanne_minority_protections__restrictive_reading, theater_ratio, 1974, 0.28).
narrative_ontology:measurement_basis(laus_tr_t1974, observed).
narrative_ontology:measurement(laus_tr_t1990, lausanne_minority_protections__restrictive_reading, theater_ratio, 1990, 0.33).
narrative_ontology:measurement_basis(laus_tr_t1990, observed).
narrative_ontology:measurement(laus_tr_t2008, lausanne_minority_protections__restrictive_reading, theater_ratio, 2008, 0.29).
narrative_ontology:measurement_basis(laus_tr_t2008, observed).
narrative_ontology:measurement(laus_tr_t2024, lausanne_minority_protections__restrictive_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(laus_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(laus_be_t1924, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1924, 0.42).
narrative_ontology:measurement_basis(laus_be_t1924, observed).
narrative_ontology:measurement(laus_be_t1942, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1942, 0.58).
narrative_ontology:measurement_basis(laus_be_t1942, observed).
narrative_ontology:measurement(laus_be_t1964, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1964, 0.71).
narrative_ontology:measurement_basis(laus_be_t1964, observed).
narrative_ontology:measurement(laus_be_t1974, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1974, 0.8).
narrative_ontology:measurement_basis(laus_be_t1974, observed).
narrative_ontology:measurement(laus_be_t1990, lausanne_minority_protections__restrictive_reading, base_extractiveness, 1990, 0.79).
narrative_ontology:measurement_basis(laus_be_t1990, observed).
narrative_ontology:measurement(laus_be_t2008, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement_basis(laus_be_t2008, observed).
narrative_ontology:measurement(laus_be_t2024, lausanne_minority_protections__restrictive_reading, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement_basis(laus_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1924, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1924, 0.48).
narrative_ontology:measurement_basis(laus_su_t1924, observed).
narrative_ontology:measurement(laus_su_t1942, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1942, 0.6).
narrative_ontology:measurement_basis(laus_su_t1942, observed).
narrative_ontology:measurement(laus_su_t1964, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1964, 0.74).
narrative_ontology:measurement_basis(laus_su_t1964, observed).
narrative_ontology:measurement(laus_su_t1974, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1974, 0.84).
narrative_ontology:measurement_basis(laus_su_t1974, observed).
narrative_ontology:measurement(laus_su_t1990, lausanne_minority_protections__restrictive_reading, suppression_requirement, 1990, 0.81).
narrative_ontology:measurement_basis(laus_su_t1990, observed).
narrative_ontology:measurement(laus_su_t2008, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement_basis(laus_su_t2008, observed).
narrative_ontology:measurement(laus_su_t2024, lausanne_minority_protections__restrictive_reading, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(laus_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Lausanne minority protections' per the epsilon-invariance principle. All three sibling readings share one referent - the standing arrangement of Turkish minority governance - but index different epsilon over it (OQ-26: reading-indexed values, shared referent), and each instantiates a structurally distinct constraint: this restrictive reading (coverage confined to individual worship, enforcement domestic, minority institutions in the victim set), the expansive reading (functional-continuity guarantees binding the state across institutional domains), and the guarantor reading (external enforceability relocating adjudication). Upstream/downstream structure: the restrictive reading exerts structural pressure on the guarantor sibling (shrinking the enforceable surface that guarantor diplomacy can reach) and stands in exclusive-or scope opposition to the expansive sibling (foreclosure relation, declared in cs_structure.reading_relations). Each file links the other two through affects_constraints; no story folds the contest into its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
