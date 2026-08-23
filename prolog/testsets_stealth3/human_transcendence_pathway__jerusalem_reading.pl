% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Pathway: Participatory Rebuilding Under Divine Blessing
 *   domain: religious/political-theological/technology-ethics
 *
 * SUMMARY:
 *   This story instantiates the Jerusalem reading of the
 *   human_transcendence_pathway kernel: the claim that authentic human
 *   community is rebuilt through patient, participatory labor under divine
 *   blessing, integrating plurality into communion rather than uniformity.
 *   Its archetype is the post-exilic rebuilding of Zion — dispersed families
 *   returning to ruins, raising walls and cisterns together, keeping gleaning
 *   edges and debt-remission cycles for those who cannot provide — and its
 *   modern analogue is the Catholic social-doctrine vision of subsidiarity
 *   and solidarity set against technocratic homogenization. The arrangement
 *   coordinates a genuine collective-action problem (reconstruction and
 *   mutual provision after dispersal) through persuasion, formation, and
 *   shared ritual rather than coercive machinery. KEY AGENTS (by structural
 *   relationship): returning_exiles — primary beneficiary
 *   (powerless/constrained), displaced returnees with first claim on shared
 *   works; resident_households — contributing beneficiaries
 *   (moderate/constrained); vulnerable_dependents — protected beneficiaries
 *   (powerless/trapped); covenant_formators — agenda-setters
 *   (institutional/identity_locked) who teach, convene, and allocate labor;
 *   sabbath_trading_merchants and regional_power_brokers — excluded outsiders
 *   (organized/mobile, powerful/mobile) who would integrate the community
 *   into larger commercial and imperial networks on their own terms;
 *   political_theology_observers — analytical observers. The claim/metric gap
 *   is deliberate and independent: the reading is CLAIMED as rope (and the
 *   structural data supports it — no victim seat exists), while the authored
 *   metrics record low-but-nonzero and slowly rising extraction, because the
 *   historical arc of such communities shows consolidation costs
 *   accumulating. The engine measures that divergence; the ε referent is the
 *   standing covenantal rebuilding arrangement as this reading assesses it,
 *   never the technocratic alternative this reading declines.
 *
 * KEY AGENTS:
 *   - returning_exiles: primary beneficiary (powerless/constrained) — displaced families rebuilding on ancestral land with first claim on shared works
 *   - resident_households: beneficiary (moderate/constrained) — established families contributing labor and first-fruits for security and standing
 *   - vulnerable_dependents: protected beneficiary (powerless/trapped) — widows, orphans, and sojourners covered by gleaning and remission
 *   - covenant_formators: agenda_setter (institutional/identity_locked) — priests, scribes, and elders who teach, convene assemblies, and organize work
 *   - sabbath_trading_merchants: excluded (organized/mobile) — traders who would fold households into regional markets seven days a week
 *   - regional_power_brokers: excluded (powerful/mobile) — governors and estate holders who prefer this population taxed and dependent
 *   - political_theology_observers: observer (analytical/analytical) — scholars comparing the pattern's durability and costs against technocratic organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.22).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Pathway: Participatory Rebuilding Under Divine Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political-theological/technology-ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '1a6add2f-4ec1-4b39-bcc7-178ce7618fbb').
narrative_ontology:cs_kernel_codification('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', fixed_text).
narrative_ontology:cs_authority_grounding('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', lineage).
narrative_ontology:cs_interpretation_layer_present('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb').
narrative_ontology:cs_reading_relation('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', foundational, plurality_constitutive_of_communion).
narrative_ontology:cs_axiom_status(plurality_constitutive_of_communion, holdable).
narrative_ontology:cs_axiom_grounding('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', plurality_constitutive_of_communion, theological).
narrative_ontology:cs_axiom('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', foundational, grace_precedes_construction).
narrative_ontology:cs_axiom_status(grace_precedes_construction, holdable).
narrative_ontology:cs_axiom_grounding('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', grace_precedes_construction, theological).
narrative_ontology:cs_reference_frame('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', covenantal_pluriform_communion).
narrative_ontology:cs_drift_state('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a6add2f-4ec1-4b39-bcc7-178ce7618fbb', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, resident_households).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, vulnerable_dependents).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, solidarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced families back on ancestral land after years away. They work the shared rebuilding projects — walls, cisterns, terraces — and hold first claim on what the projects provide: housing sites, water, seed grain. Leaving again would mean another round of dispossession, so they stay and build even when the pace is slow and the harvest thin.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    powerless, generational, constrained, regional).

% Established families who never left, or returned earlier. They contribute labor days, first-fruits, and materials to common works, attend the covenant assemblies, and take turns hosting shared meals. Their return is security, working infrastructure, and standing among neighbors; withdrawing would cost them the goodwill their households run on.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, resident_households, beneficiary,
    moderate, generational, constrained, local).

% Widows, orphans, resident foreigners, and others who cannot field laborers. They gather at the edges of harvested fields, receive shares at festival tables, and are covered by the remission cycles that cancel debts every seventh year. They have nowhere else to go; the community's habits of provision are the whole of their safety.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, vulnerable_dependents, beneficiary,
    powerless, biographical, trapped, local).

% Priests, scribes, and elders who teach the law, convene the assemblies, keep the feast calendar, settle disputes, and organize work rosters. They are supported by tithes and offerings — enough to live on, tied to their service. Their vocation, reputation, and entire social world are bound up in the community's continuing life; stepping aside would mean losing not a post but their place and sense of who they are.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, covenant_formators, agenda_setter,
    institutional, generational, identity_locked, regional).

% Traders moving goods along the regional roads who would sell at the town gates every day of the week. They see idle workshops and unsold surplus where the community sees kept rest, and they offer prices and credit that would fold individual households into wider markets. Most operate out of caravan towns beyond the community's bounds and can take their business elsewhere freely.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, sabbath_trading_merchants, excluded,
    organized, immediate, mobile, regional).

% Governors, garrison commanders, and estate holders in the surrounding province. They would prefer this population pliable: taxed, conscriptable, dependent on imperial grain and credit. A town that provisions itself, guards its own walls, and answers to its own assemblies offers them nothing to administer and much to distrust; their attention simply moves to more governable districts.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, regional_power_brokers, excluded,
    powerful, biographical, mobile, continental).

% Scholars of religion, social theorists, and ethicists who study communities of this kind, ancient and contemporary, comparing their durability and their costs against societies organized around technical systems and markets. They hold no stake in the community's fortunes and can name failure modes that participants cannot see from inside.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, political_theology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__jerusalem_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rebuilds common life after dispersal: pools household labor for infrastructure no family could build alone (walls, cisterns, terraces), guarantees provision for members who cannot provide for themselves, and transmits formed character and shared memory to the next generation through teaching and feast — deliberately without concentrating decisions in a center.
% TRANSFER_FUNCTION: Moves labor days, first-fruits, and tithes from every household into shared works, the support of teachers, and provision for the destitute; moves belonging, honor, and a named place in the assembly to every contributor; moves formation from elders to children. What it deliberately does not move: decision-making upward, or surplus into private accumulation.
% ABSENT_VOICES: Merchants and acceleration-minded households who would trade through the week and plug the town into regional credit; provincial administrators who would rather collect taxes than watch a self-provisioning town; and residents scarred by earlier coercive religion who want community without covenant discipline. They sit in caravan towns, garrison offices, and quiet corners of the assembly respectively.
% DISAPPEARANCE_RATIONALE: An overnight loss would strand the dependent — no gleanings, no remission, no festival shares — halt every shared work mid-course, and push households one by one into regional markets and patronage networks for credit and protection; within a generation the settlement would resemble any other tributary village, and the formators' entire vocation would dissolve with it.
% FOUNDING_PROBLEM: A scattered, traumatized people returning to ruins: no working institutions, fields overgrown, neighbors hostile, and the memory of empire's enforced uniformity — the very machine that had scattered them — still the region's default model for getting things done.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the post-exilic province attest the reconstruction conditions from outside any confessional interest; contemporary sociologists of loneliness and institutional distrust — many indifferent or hostile to the theological claim — corroborate that the underlying problem (dispersal, atomization, dependence on impersonal systems) persists in modern form; and the technocratic camp itself concedes the social fabric is fraying, disputing only the remedy. No corroboration exists solely from within the benefiting parties.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.22 at interval end) because real transfers exist — tithes, first-fruits, mandatory labor days — but nearly all of it returns to the contributors as walls, water, provision for the destitute, and formed successors; the residual is the bounded subsistence support of the formators, designed into the arrangement rather than captured by it. Suppression is low (0.15) because persistence runs on persuasion, liturgical formation, and covenant renewal rather than enforcement machinery; the suppression that does exist is predominantly internalized (formed desire, honor dynamics — roughly two-thirds) with a minority structural component (gate discipline, exclusion from the assembly). Theater stays low (0.10) because the outputs are inspectable: a wall either stands or does not, a harvest either feeds the dependent or does not; ritual serves the labor rather than substituting for it. Accessibility_collapse is moderate-low (0.35): the alternatives — market integration, imperial patronage, diaspora assimilation, technocratic organization — remain fully available and are visibly exercised by the excluded seats. Resistance is low-moderate (0.20): internal grumbling over debt burdens during bad harvests, elite reluctance to do manual work, merchant friction at the gates, external mockery — real, but insufficient to threaten the pattern. The temporal series run on one shared six-point grid (t=0,8,16,24,32,40) covering the first two generations of a rebuilding community, anchored to the documented post-exilic arc. The suppression_requirement series is authored deliberately rather than left static: the story specifically tracks enforcement-capacity growth, because consolidation historically added real enforcement instruments (commerce reforms, marriage regulations, temple levies) — a maturing of suppressive machinery, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   There is no structural payer seat in this arrangement, so seat divergence runs along the insider/outsider line and across horizons rather than between exploiter and exploited. From the household seats the pattern is experienced as gift: security, belonging, and a named place in the assembly purchased with visible, finite labor. From the covenant_formators' seat the same structure reads differently — they carry the administration, the dispute load, and the accountability for the calendar, and their exit is identity_locked in the strict sense: institutional identity fusion, in which the formator has become the vocation, so that stepping aside means losing not a position but a self. From the excluded seats the identical boundary appears as pure cost: the merchant sees foregone market days, the provincial administrator an ungovernable town. Same-level divergence also appears among nominal equals: resident_households and returning_exiles hold similar formal standing, but the returnees' lack of alternative land and networks makes their participation less choosable — the same covenant binds them more tightly. The engine computes these divergent experiences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary groups anchor the low-directionality end: returning_exiles and vulnerable_dependents receive the arrangement's protections with the least ability to reciprocate in kind (trapped or constrained exit places them deep on the subsidized side), and resident_households sit near symmetric — substantial contributions, substantial returns. The covenant_formators administer the arrangement; the derivation places them low-to-mid, which is descriptively right: they bear real formation labor and receive only bounded subsistence support, not accumulated surplus. The excluded seats (merchants, power brokers) fall outside the beneficiary/victim derivation entirely; they are not governed by the arrangement but shaped by its existence, and per the R3 ruling their authored absence remains commentary-grade rather than driving classification. On the receipt surface: gain_flow is authored as diffuse as an affirmative, checked claim — each named seat was examined, and none captures the arrangement's surplus. The formators' tithe support is subsistence-for-service written into the design, not accrued rent; the surplus returns to shared works and provision for the dependent. fixing_cost is authored prohibitive on independent evidence: dissolving the pattern is trivially easy (neglect suffices), but restoring dissolved trust, rebuilt commons, and formed character is generation-scale work — the historical record shows communities that lost this charism rarely recovered it. That cost asymmetry reflects slow social capital, not administrative inertia: there is no agenda-setter profiting from the arrangement's decay, which is what would distinguish a degraded-inertial case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a dispersed, traumatized people rebuilds common life without replicating the imperial uniformity that scattered it — remains live in transformed form: displacement, atomization, and dependence on impersonal technical systems are the contemporary face of the same problem, so no mandatrophy is declared and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, the consistent non-zombie configuration. The classification guards against two opposite misreadings. First, it prevents the low extraction score from being heard as 'no cost': the efficiency and scale forgone by the slow participatory path are real, but they are a chosen trade-off priced into participation, not extraction imposed on a victim class — collapsing that distinction would misread every deliberate asceticism as exploitation. Second, it prevents the sacred framing from immunizing the arrangement against scrutiny: the measurement series records extraction and enforcement capacity creeping upward with consolidation, and the institutionalization omega names the documented historical endpoint (temple-state hardening) toward which that creep tends. The rope claim is thus falsifiable in principle: if the capture omega resolves affirmatively in a given instantiation, the computed classification at household seats should migrate, and the corpus will show it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutionalization_capture_risk,
    'Does the participatory pattern survive institutional consolidation, or does clerical and elite mediation gradually convert participation into hierarchy and tithes into rent?',
    'Compare material flows and decision authority across successive generations of consolidated communities; the post-exilic historical record (temple tolls, marriage and commerce reforms enforced by gate closure, later priestly taxation) provides a documented test series.',
    'If capture consolidates, the effective operation at household seats shifts sharply upward in extraction and the computed classification migrates from coordination toward hybrid or extractive types despite unchanged founding rhetoric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutionalization_capture_risk, empirical, 'Risk that voluntary participation hardens into clerical rent collection over generations.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (jerusalem_reading) of the human_transcendence_pathway kernel; which structural element separates it from the sibling readings, and how would classification change under a sibling''s framework?',
    'Not resolvable by data alone: resolved by which framework a party adopts. Cross-reading comparison via the linked sibling stories (babel_reading, technocratic_vs_incarnational_reading). The disagreement is located in two elements: the locus of security (divine blessing versus unified technical power) and the status of plurality (resource to be integrated versus obstacle to be optimized away).',
    'Under the babel_reading the victim set relocates (those flattened by unification bear the costs) and extraction rises substantially; under the technocratic horn of the third sibling, vulnerability becomes a defect to eliminate and the marginalized lose their protective seat entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: reading-indexed classification over a shared kernel.').

omega_variable(
    voluntariness_depth_of_participation,
    'How much of observed participation is genuinely free rather than socially compelled through honor dynamics, covenant sanctions, and economic dependence on the commons?',
    'Track the trajectories of leavers and the formally excluded: if exit reliably carries loss of livelihood, standing, and community without comparable replacement, structural suppression is understated by the scalar.',
    'A high compelled-participation share raises effective suppression and pushes the computed classification toward hybrid coordination/extraction; a low share confirms the persuasion-and-formation profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_depth_of_participation, empirical, 'Whether low measured suppression reflects real freedom or unmeasured social compulsion.').

omega_variable(
    efficiency_sacrifice_status,
    'Is the forgone efficiency and scale of the slow participatory path a cost imposed on those who would prefer acceleration, or a price participants knowingly accept?',
    'Not resolvable by data alone: depends on evaluative weights. Evidence that informs it includes revealed preference (do accelerated households defect when regional markets open?) and deliberative testimony from insiders who chafe at the pace.',
    'If judged imposed, an internal victim class exists (the unconvinced within), moving the structure toward hybrid coordination/extraction; if judged an accepted trade-off, the coordination reading stands with the sacrifice priced into participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_sacrifice_status, preference, 'Whether the solidarity-for-efficiency trade is imposition or consent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jerusalem_reading_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t0, observed).
narrative_ontology:measurement(jerusalem_reading_tr_t8, human_transcendence_pathway__jerusalem_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t8, observed).
narrative_ontology:measurement(jerusalem_reading_tr_t16, human_transcendence_pathway__jerusalem_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t16, observed).
narrative_ontology:measurement(jerusalem_reading_tr_t24, human_transcendence_pathway__jerusalem_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t24, observed).
narrative_ontology:measurement(jerusalem_reading_tr_t32, human_transcendence_pathway__jerusalem_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t32, observed).
narrative_ontology:measurement(jerusalem_reading_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(jerusalem_reading_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jerusalem_reading_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(jerusalem_reading_be_t0, observed).
narrative_ontology:measurement(jerusalem_reading_be_t8, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement_basis(jerusalem_reading_be_t8, observed).
narrative_ontology:measurement(jerusalem_reading_be_t16, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement_basis(jerusalem_reading_be_t16, observed).
narrative_ontology:measurement(jerusalem_reading_be_t24, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement_basis(jerusalem_reading_be_t24, observed).
narrative_ontology:measurement(jerusalem_reading_be_t32, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement_basis(jerusalem_reading_be_t32, observed).
narrative_ontology:measurement(jerusalem_reading_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement_basis(jerusalem_reading_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jerusalem_reading_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.04).
narrative_ontology:measurement_basis(jerusalem_reading_su_t0, observed).
narrative_ontology:measurement(jerusalem_reading_su_t8, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 8, 0.06).
narrative_ontology:measurement_basis(jerusalem_reading_su_t8, observed).
narrative_ontology:measurement(jerusalem_reading_su_t16, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement_basis(jerusalem_reading_su_t16, observed).
narrative_ontology:measurement(jerusalem_reading_su_t24, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 24, 0.11).
narrative_ontology:measurement_basis(jerusalem_reading_su_t24, observed).
narrative_ontology:measurement(jerusalem_reading_su_t32, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 32, 0.13).
narrative_ontology:measurement_basis(jerusalem_reading_su_t32, observed).
narrative_ontology:measurement(jerusalem_reading_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(jerusalem_reading_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how humanity secures its flourishing beyond itself' decomposes into three structurally distinct readings of the human_transcendence_pathway kernel. This story instantiates jerusalem_reading only (epsilon low-moderate, no structural victim class, persuasion-based persistence). babel_reading carries a different victim set (those flattened by enforced unification) and materially higher epsilon; technocratic_vs_incarnational_reading splits further on the locus of transcendence. The upstream/downstream gradient runs from this reading outward: the Jerusalem pattern's demonstrated viability is cited as evidence AGAINST the babel premise, which is why the influence edge points toward the technocratic contest. Each member links the others via affects_constraints; classification is per-file and never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
