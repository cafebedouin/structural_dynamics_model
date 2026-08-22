% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Instrument under Subsidiary Governance (Instrumental-Subsidiarity Reading)
 *   domain: religious-social-teaching/technology-ethics/political-theology
 *
 * SUMMARY:
 *   The instrumental_subsidiarity reading instantiates a governance
 *   settlement in which artificial intelligence is treated as a morally
 *   neutral instrument: the technology itself carries no moral valence,
 *   responsibility for its effects attaches at the point of use, and
 *   protection of human dignity is delegated to legal frameworks, ethics
 *   review, and transparency requirements allocated across levels of
 *   authority by the principle of subsidiarity. The standing arrangement
 *   under contest is this settlement as it has actually operated since the
 *   mid-2010s (interval 0-12 maps approximately to 2014-2026): principles
 *   documents, ethics boards, risk-tiered regulation, and liability regimes
 *   that govern AI use while leaving design-level choices formally
 *   unregulated. The ε authored here is assessed on that standing arrangement
 *   by this reading's own lights: the reading affirms the settlement's
 *   legitimacy while acknowledging that regulation lags deployment, that
 *   transparency requirements are weakly enforced, and that harms to decision
 *   subjects accumulate in the accountability gap the settlement leaves open.
 *   This story is one member of a three-reading constraint family; the
 *   decomposition record is in network.dual_formulation_note and
 *   commentary.kernel_context.
 *
 * KEY AGENTS:
 *   - ai_developing_enterprises: primary beneficiary (powerful/arbitrage) — collects the liability shield and regulatory legitimacy the neutrality premise provides; arbitrages across jurisdictions
 *   - algorithmic_decision_subjects: primary target (powerless/trapped) — bears the harms of welfare, hiring, lending, and policing algorithms while accountability diffuses across the governance chain
 *   - automatable_sector_workers: secondary target (powerless/constrained) — absorbs displacement costs the settlement treats as use-case externalities
 *   - local_administrative_bodies: dual-positioned (moderate/constrained) — receives delegated regulatory competence together with the unfunded burden of exercising it
 *   - applied_ethics_apparatus: beneficiary (moderate/identity_locked) — collects standing, funding, and careers from the ethics-review layer the settlement creates
 *   - legislative_regulatory_bodies: agenda_setter (institutional/constrained) — codifies and administers the framework
 *   - cst_magisterium: normative agenda_setter (institutional/identity_locked) — articulates the reading for its constituency through social doctrine
 *   - global_south_data_subjects: excluded victim (powerless/trapped) — supplies training data and bears deployment externalities with no seat where the settlement is struck
 *   - political_theologians: analytical observer (analytical/analytical) — sees the full structure of the settlement and its alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.58).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.55).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Instrument under Subsidiary Governance (Instrumental-Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "religious-social-teaching/technology-ethics/political-theology").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'bad2bb37-0448-4b91-934f-7db13e735988').
narrative_ontology:cs_kernel_codification('bad2bb37-0448-4b91-934f-7db13e735988', formalized).
narrative_ontology:cs_authority_grounding('bad2bb37-0448-4b91-934f-7db13e735988', lineage).
narrative_ontology:cs_interpretation_layer_present('bad2bb37-0448-4b91-934f-7db13e735988').
narrative_ontology:cs_reading_relation('bad2bb37-0448-4b91-934f-7db13e735988', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('bad2bb37-0448-4b91-934f-7db13e735988', ai_human_relationship__technocratic_optimization, influences).
narrative_ontology:cs_axiom('bad2bb37-0448-4b91-934f-7db13e735988', foundational, technology_morally_neutral_in_itself).
narrative_ontology:cs_axiom_status(technology_morally_neutral_in_itself, holdable).
narrative_ontology:cs_axiom_grounding('bad2bb37-0448-4b91-934f-7db13e735988', technology_morally_neutral_in_itself, deontological).
narrative_ontology:cs_axiom('bad2bb37-0448-4b91-934f-7db13e735988', foundational, subsidiarity_allocates_governance_competence).
narrative_ontology:cs_axiom_status(subsidiarity_allocates_governance_competence, holdable).
narrative_ontology:cs_axiom_grounding('bad2bb37-0448-4b91-934f-7db13e735988', subsidiarity_allocates_governance_competence, conventional).
narrative_ontology:cs_reference_frame('bad2bb37-0448-4b91-934f-7db13e735988', instrumental_neutrality_subsidiary_governance).
narrative_ontology:cs_drift_state('bad2bb37-0448-4b91-934f-7db13e735988', contemporary_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bad2bb37-0448-4b91-934f-7db13e735988', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_developing_enterprises).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, applied_ethics_apparatus).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, local_administrative_bodies).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, automatable_sector_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, global_south_data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, local_administrative_bodies).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, ai_instrumentality_premise).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, legal_protection_of_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy AI systems across markets. The settlement treats their products as neutral instruments, so accountability for design choices routes to users, deployers, and regulators rather than to development decisions; liability doctrine built on the tool framing shields them from design-level claims. They can shift development and deployment across jurisdictions, arbitraging the gaps between regulatory regimes, and they fund many of the ethics and standards bodies that elaborate the settlement.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developing_enterprises, beneficiary,
    powerful, biographical, arbitrage, global).

% Are scored, ranked, screened, and flagged by AI systems in welfare eligibility, hiring, lending, insurance, immigration, and policing. They did not choose these systems and cannot opt out of them; when decisions go wrong they face a chain of vendors, deployers, and agencies each locating fault elsewhere, with the tool framing placing responsibility anywhere but in design. Exit means leaving the service, the labor market, or the jurisdiction.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmic_decision_subjects, payer,
    powerless, biographical, trapped, global).

% Work in sectors where AI deployment substitutes for tasks — warehousing, transport back offices, customer service, translation, routine analysis. The settlement treats displacement as a use-case outcome to be remediated by retraining and adjustment policy rather than a design decision; they bear the transition costs while holding no seat in deployment choices. Exit is bounded: retraining leads to other exposed sectors, and mobility is limited by housing, family, and credentials.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, automatable_sector_workers, payer,
    powerless, biographical, constrained, regional).

% Municipalities, regional agencies, and sectoral inspectorates receive delegated authority to administer AI rules — registering systems, auditing conformity, handling complaints. The delegation confirms their procedural role, but arrives without commensurate funding or technical staff, so they absorb the implementation burden of rules they did not write and cannot renegotiate. Withdrawing from administration would mean ceding the role upward or to private auditors.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, local_administrative_bodies, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, local_administrative_bodies, payer).

% Ethics boards, responsible-AI offices, audit firms, and standards consultants whose standing, funding, and careers exist because governance routes through ethics review. Their professional identity is fused with the review frame: if responsibility relocated wholesale to design-level mandates, their function would be absorbed into engineering compliance and their distinct role would dissolve. Leaving would mean abandoning the field they are credentialed in.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, applied_ethics_apparatus, beneficiary,
    moderate, biographical, identity_locked, global).

% Parliaments, the European Commission, and national agencies codify and administer the settlement: risk-tiered regulation, transparency obligations, conformity assessment. They could in principle replace it with design-level mandates, but face industry lobbying, international competitiveness arguments, and the technical difficulty of regulating development directly. Their frameworks are written to outlast electoral cycles.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, legislative_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, continental).

% The teaching office of the Catholic Church and its social-doctrine apparatus articulate the settlement for their constituency: the Rome Call for AI Ethics (2020) and Antiqua et nova (2025) affirm technology's instrumental character while insisting on human dignity, subsidiarity, and legal protection. The office cannot abandon its social doctrine without dissolving the teaching authority that issues it; it reaches globally through dioceses, universities, and multi-signatory initiatives.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, cst_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Populations whose data trains the models and whose regions absorb deployment externalities — content-moderation labor, extractive data practices, exported algorithmic systems governing their institutions — but who hold no seat in the venues where the settlement is negotiated, which concentrate in the EU and US. Under the tool framing their data and exposure are priced as inputs; the governance conversation proceeds without them, and exit from data extraction is effectively unavailable.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, global_south_data_subjects, excluded,
    powerless, biographical, trapped, global).

% Scholars working across theology, law, and technology studies who analyze how the settlement allocates responsibility, how subsidiarity distributes competence and burden, and what rival framings of the technology would each demand. They collect nothing and bear nothing under the arrangement; their seat is analytical.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, political_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, ai_developing_enterprises).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of governing AI deployment across morally plural societies: by fixing the technology's status as instrument, it lets law, ethics review, and transparency requirements proceed without first resolving contested metaphysics; subsidiarity allocates each decision to the lowest level of competent authority, keeping governance close to affected contexts.
% TRANSFER_FUNCTION: Moves responsibility and cost downward and outward: design-level accountability transfers from developers to users, deployers, and regulators; implementation burdens transfer to local administrations; harm costs concentrate on decision subjects and displaced workers; deployment gains — capability, productivity, data position — accrue to developing enterprises.
% ABSENT_VOICES: Global South data subjects and communities bearing deployment externalities are absent from the negotiating venues. Voices holding that technology itself requires moral ordering are not absent from the wider tradition, but the settlement's procedural architecture has no slot for contesting the tool's character — a neutral-instrument premise can process disputes about use but not disputes about what the technology is.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, AI governance would reorganize around one of the rival framings: design-level mandates and licensing if the ordering reading took the field, unregulated optimization if the efficiency framing did. Liability doctrine would be rewritten; ethics boards and conformity-assessment bodies would lose their charter; local administrations would shed or gain duties. Every seated party's arrangements depend on the settlement's specific allocation of responsibility and burden.
% FOUNDING_PROBLEM: In the mid-2010s, machine-learning systems began making consequential decisions about people faster than any shared account of the technology's moral status could form. The settlement was built to let governance proceed under that disagreement: fix the technology as a neutral instrument, route responsibility to use and oversight, and allocate decisions by subsidiarity.
% FOUNDING_PROBLEM_CORROBORATION: The governance gap the settlement answered is corroborated from outside the benefiting parties: intergovernmental analyses (OECD, UNESCO, Council of Europe) document deployment outrunning oversight, and the litigation record of algorithmic-harm plaintiffs documents the accountability gap it was meant to close. What no outside party corroborates is the stronger claim that neutrality-plus-subsidiarity is the necessary or sufficient basis for closing that gap — that claim appears only in documents issued by the settlement's own authors and beneficiaries (industry principles, Rome Call signatories, the ethics apparatus).
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising from 0.38 across the interval): the neutrality premise deflects design-level accountability, and the deflection scales with deployment — as more consequential decisions route through AI systems, more harm-cost lands on parties who chose nothing, while the settlement's own terms locate fault at the use point. Suppression is 0.55 and is structural, not internalized: venue control (the settlement's negotiating and administering bodies), liability doctrine that forecloses design-level claims, and the framing's dominance in regulatory discourse marginalize design-level alternatives; no interpersonal or internalized mechanism is claimed, so no suppression-mechanism omega is required. Theater is 0.55: a large and growing share of the settlement's activity — principles documents, ethics boards without enforcement power, transparency reports — performs justification rather than altering deployments, and ethics-washing became standard practice as deployment scaled. Accessibility_collapse is 0.40: alternatives remain thinkable and live — both rival readings persist as organized positions — the settlement makes design-level mandates procedurally hard rather than unthinkable. Resistance is 0.60: the settlement is contested from three directions at once (industry against regulation, harmed parties for design-level accountability, ordering-readings against neutrality itself). The suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: the machinery administering the settlement (risk-tiered statutes, conformity assessment, national AI offices) was built up over the interval, and holding the framing against rising structural critique required more active work. All three series run on one shared time grid (0,2,4,6,8,10,12) so every metric is authored at every examined point. Coalition note: the victim seats are individually powerless, but decision subjects and exposed workers share the same accountability grievance and have demonstrated coalition capacity (algorithmic-plaintiff networks, union campaigns on workplace AI); the settlement's persistence partly depends on keeping that coalition procedural rather than structural.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the developing enterprises' position the settlement is legitimate coordination they helped build: a workable pluralist framework that lets deployment proceed under law. From the trapped decision subjects' position the same structure operates as accountability that never arrives — each harm is real, and each is routed to a seat that did not cause it. The magisterium seat experiences the settlement as procedural fidelity to subsidiarity and dignity; the local administrative bodies experience the same principle as an unfunded mandate. Among same-power moderate actors, the ethics apparatus (identity-locked, collecting) and local administrations (burden-bearing) hold the same power atom but opposite relationships to the settlement's burden — differentiated by exit options, not power. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: developing enterprises (neutrality premise as liability shield, arbitrage-grade exit drives them to the beneficiary end of d), the ethics apparatus (collects standing and funding, identity-locked, low d), and local administrative bodies (declared beneficiary via delegated competence). Victims declared: decision subjects (trapped, near full-target d), displaced workers (constrained, high d), and Global South data subjects (trapped and unseated, high d). One honest divergence from the derivation: local_administrative_bodies are declared beneficiaries, but their true position is near symmetric (d ≈ 0.45) because the delegated competence arrives without capacity funding — they collect a role and pay an unfunded burden through the same delegation. The override surface is keyed to power atoms, and the only other moderate-power agent (the ethics apparatus) sits genuinely near the beneficiary end, so a moderate-atom override would misplace it; overrides are therefore omitted and the dual position is carried here and in the stakeholder situation text.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabels. Reading the settlement as pure coordination would whitewash the accountability deflection that lets deployers externalize harm costs onto trapped parties while collecting the settlement's legitimacy benefits — that is the extraction half, and it requires active enforcement (liability doctrine, venue control, lobbying) to hold. Reading it as pure extraction would erase the genuine coordination achievement: governing AI deployment under moral pluralism, without a shared metaphysics of technology, through subsidiarity's competence allocation — an achievement neither rival reading currently provides. The founding problem (governing deployment that outruns moral consensus) is still live even as the neutrality premise that operationalized it is contested, so founding_problem_status is 'contested' against a 'world_rearranges' verdict: the arrangement persists because the problem persists, not because the problem died and the form outlived it — no zombie flag should fire from this story. Identity-lock dynamics: the ethics apparatus is bound by professional identity fusion (careers credentialed inside the review frame; exit dissolves the role), and the magisterium by institutional identity fusion (the social doctrine and the teaching authority are mutually constitutive); if either frame broke, that seat's relationship to the settlement would invert rather than merely weaken.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is one reading of the kernel ai_human_relationship — the instrumental_subsidiarity reading. How would the constraint''s structure change under each sibling reading, and where exactly do the readings disagree?',
    'Generate the sibling reading stories (incarnational_humanism, technocratic_optimization) with their own ε, beneficiary/victim structures, and stakeholders; compare per-seat classifications across the family. The disagreement is located at a single structural element: the moral status of the technology itself, which determines where responsibility attaches.',
    'Under incarnational_humanism the neutrality axiom is denied and responsibility relocates to design-level ordering — the victim set widens to include technology''s formative effects on persons, and ε for the standing deployment arrangement rises. Under technocratic_optimization the regulatory safeguard is removed and extraction becomes direct and unmediated, with human value itself priced by optimization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Committer structure: which kernel, which reading this story instantiates, and what each sibling would change structurally.').

omega_variable(
    neutrality_premise_stability,
    'Does the instrumentality premise remain stable as AI systems become agentic and goal-directed?',
    'Track whether deployed systems exhibit behavior that resists the tool metaphor (autonomous goal pursuit, emergent capability, resistance to user control) and whether liability doctrine continues to treat them as instruments.',
    'If the tool metaphor fails empirically, the reading''s foundational axiom weakens and responsibility-allocation shifts toward design — the settlement''s extraction component rises and its coordination claim narrows to a residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_premise_stability, empirical, 'Empirical stability of the moral-neutrality premise under agentic AI.').

omega_variable(
    subsidiarity_capacity_mismatch,
    'Does delegation to the lowest competent level actually protect human dignity, or does it shift regulatory burden onto levels without technical capacity?',
    'Comparative audit of implementation capacity versus delegated competence across member states and municipalities administering AI rules — staffing, technical expertise, and complaint-resolution rates against delegated duties.',
    'A systematic capacity mismatch would reclassify the subsidiarity component as burden-shifting: extraction rises, the procedural safeguard reads as regulatory offloading, and the settlement drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_capacity_mismatch, empirical, 'Whether subsidiarity functions as a safeguard or as an offloading mechanism.').

omega_variable(
    ethics_apparatus_function,
    'Does the ethics-review layer constrain deployments or perform justification for them?',
    'Track the rate at which ethics-board and responsible-AI recommendations alter or halt deployments versus the rate at which they are absorbed into marketing and compliance artifacts.',
    'High absorption would push theater_ratio further up and mark the ethics component as piton-drifting — theatrical maintenance of a governing function it no longer performs — which would narrow the settlement''s genuine coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethics_apparatus_function, empirical, 'Constraint versus theater in the AI ethics apparatus.').

omega_variable(
    persistence_driver,
    'Is the settlement''s persistence driven by the live founding problem (governing deployment under pluralism) or by beneficiary entrenchment (the liability shield the neutrality premise provides industry)?',
    'Counterfactual comparison: model governance outcomes if the neutrality premise were replaced by design-level mandates while subsidiarity''s competence allocation is retained, versus the status quo, on harm remediation and accountability closure.',
    'If entrenchment dominates, the settlement drifts toward snare despite the live founding problem; if the problem dominates, the tangled_rope classification holds and the settlement''s coordination function is load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_driver, empirical, 'Whether persistence tracks the founding problem or beneficiary capture.').

omega_variable(
    authority_framing_ambiguity,
    'Is the settlement''s authority grounded in the CST lineage that articulates it (magisterial social doctrine as interpretive chain), or in diffuse epistemic acceptance of the instrumentality premise across policy discourse?',
    'Trace the citational structure of the regulatory frameworks: do statutes and agency guidance cite CST documents and the Rome Call lineage, or independent instrumentalist reasoning in law and economics? If the latter dominates, the lineage framing overstates the magisterium''s adjudicating role.',
    'Under a diffuse_epistemic grounding, interpretation_layer_present becomes invalid and the reading''s drift dynamics change — drift would track policy-discourse consensus rather than magisterial reception, and the identity-locked magisterium seat would lose its agenda-setting weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_ambiguity, conceptual, 'Two defensible authority framings for the same settlement; the choice changes the commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(ai_h_tr_t0, observed).
narrative_ontology:measurement(ai_h_tr_t2, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 2, 0.33).
narrative_ontology:measurement_basis(ai_h_tr_t2, observed).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 4, 0.37).
narrative_ontology:measurement_basis(ai_h_tr_t4, observed).
narrative_ontology:measurement(ai_h_tr_t6, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 6, 0.41).
narrative_ontology:measurement_basis(ai_h_tr_t6, observed).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.45).
narrative_ontology:measurement_basis(ai_h_tr_t8, observed).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(ai_h_tr_t10, observed).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 12, 0.55).
narrative_ontology:measurement_basis(ai_h_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ai_h_be_t0, observed).
narrative_ontology:measurement(ai_h_be_t2, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(ai_h_be_t2, observed).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(ai_h_be_t4, observed).
narrative_ontology:measurement(ai_h_be_t6, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(ai_h_be_t6, observed).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.53).
narrative_ontology:measurement_basis(ai_h_be_t8, observed).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(ai_h_be_t10, observed).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(ai_h_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ai_h_su_t0, observed).
narrative_ontology:measurement(ai_h_su_t2, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 2, 0.42).
narrative_ontology:measurement_basis(ai_h_su_t2, observed).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 4, 0.45).
narrative_ontology:measurement_basis(ai_h_su_t4, observed).
narrative_ontology:measurement(ai_h_su_t6, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(ai_h_su_t6, observed).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(ai_h_su_t8, observed).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(ai_h_su_t10, observed).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(ai_h_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).

% DUAL FORMULATION NOTE:
% The colloquial question 'how should AI relate to human ends' decomposes, per the ε-invariance principle, into three structurally distinct constraints — one per reading of the ai_human_relationship kernel. The instrumental_subsidiarity reading (this file) holds technology morally neutral and locates responsibility in use, regulation, and ethics review allocated by subsidiarity; its ε (0.58) is assessed on the standing governance settlement by its own lights. The incarnational_humanism reading denies neutrality and orders technology to integral human development — its ε is assessed on the same deployment arrangement but as a violation of ordering, with a wider victim set. The technocratic_optimization reading removes the regulatory safeguard and prices human value by optimization — its ε is assessed on a different referent entirely. These are not one constraint viewed from three angles: different beneficiary/victim structures, different failure modes, different research and advocacy communities. This file links to both siblings via affects_constraints; the upstream instrumental settlement influences the technocratic sibling's operating environment (regulatory lag subsidizes optimization-first deployment) while coexisting with the incarnational sibling as a live rival within the same tradition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
