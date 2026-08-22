% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Fragmentation Arrangement (Theological Reading)
 *   domain: religious_history/political_economy
 *
 * SUMMARY:
 *   Between the Augsburg Confession (t=0, 1530) and the Peace of Westphalia
 *   (t=120, 1650), the Western church reorganized into mutually exclusive
 *   confessional blocs whose boundaries were fixed in documents — the Book of
 *   Concord, the Canons of Dort, the Westminster Standards, the Tridentine
 *   decrees — and enforced by subscription, consistory discipline, heresy
 *   statute, and censorship. This story instantiates ONE reading of the
 *   reformation_composite kernel: the theological_fragmentation_reading, for
 *   which doctrinal pluralism is the primary observable, confessional
 *   documents are the constraint artifacts, and denominational leadership is
 *   the beneficiary of fragmentation. The standing arrangement under
 *   assessment — the epsilon referent — is the confessional boundary system
 *   itself, assessed by this reading's own lights: the commitments are
 *   genuinely held and genuinely incompatible, the coordination they perform
 *   is real, and the burdens they impose are real. The sibling readings
 *   (political_realignment_reading, technological_mediation_reading)
 *   instantiate different standing arrangements with their own epsilon values
 *   and are linked as a constraint family, not merged into this file. KEY
 *   AGENTS (by structural relationship): - denominational_leadership:
 *   Agenda-setting beneficiary (institutional/identity_locked) — administers
 *   confessional subscription and collects its revenues -
 *   university_theology_faculties: Secondary beneficiary
 *   (organized/constrained) — staffs orthodoxy examination from endowed
 *   chairs - ordinary_parish_laity: Primary target (powerless/trapped) —
 *   bears tithes, compulsory conformity, and conscription into confessional
 *   war - religious_dissenters: Extreme target (powerless/constrained) —
 *   persecuted by every confession alike - confessional_minorities:
 *   Structured target (moderate/constrained) — tolerated by edict, narrowed
 *   by law - irenical_moderates: Excluded voice (moderate/constrained) —
 *   comprehension proposals kept out of the settlements -
 *   ecclesiastical_historians: Analytical observer (analytical/analytical) —
 *   sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.72).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.78).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Fragmentation Arrangement (Theological Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '759e4fd8-087c-4d58-a9ce-537736c69105').
narrative_ontology:cs_kernel_codification('759e4fd8-087c-4d58-a9ce-537736c69105', fixed_text).
narrative_ontology:cs_authority_grounding('759e4fd8-087c-4d58-a9ce-537736c69105', lineage).
narrative_ontology:cs_interpretation_layer_present('759e4fd8-087c-4d58-a9ce-537736c69105').
narrative_ontology:cs_reading_relation('759e4fd8-087c-4d58-a9ce-537736c69105', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('759e4fd8-087c-4d58-a9ce-537736c69105', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('759e4fd8-087c-4d58-a9ce-537736c69105', foundational, soteriological_commitments_generate_structural_incompatibility).
narrative_ontology:cs_axiom_status(soteriological_commitments_generate_structural_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('759e4fd8-087c-4d58-a9ce-537736c69105', soteriological_commitments_generate_structural_incompatibility, empirically_contingent).
narrative_ontology:cs_axiom('759e4fd8-087c-4d58-a9ce-537736c69105', foundational, doctrinal_difference_legitimately_church_dividing).
narrative_ontology:cs_axiom_status(doctrinal_difference_legitimately_church_dividing, holdable).
narrative_ontology:cs_axiom_grounding('759e4fd8-087c-4d58-a9ce-537736c69105', doctrinal_difference_legitimately_church_dividing, deontological).
narrative_ontology:cs_reference_frame('759e4fd8-087c-4d58-a9ce-537736c69105', confessional_doctrinal_settlement).
narrative_ontology:cs_drift_state('759e4fd8-087c-4d58-a9ce-537736c69105', contemporary_ecumenical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('759e4fd8-087c-4d58-a9ce-537736c69105', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, university_theology_faculties).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, ordinary_parish_laity).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religious_dissenters).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, confessional_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, ordinary_parish_laity).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, scriptural_perspicuity_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, doctrinal_non_negotiability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Superintendents, bishops, consistories, and synod officers define the confessional standard, examine and ordain clergy against it, run visitation and discipline, and collect the tithes, stipends, fees, and endowments that flow to confessional office. Their orders, offices, and livelihoods exist only inside their own confession; a Lutheran superintendent who adopted Reformed teaching on the Eucharist would lose pulpit, income, and standing at a stroke, so departure is not a live option however much they might privately admire an opponent's learning.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% Endowed chairs at Wittenberg, Geneva, Heidelberg, Douai, and their peers are funded on confessional terms; faculties draft the formulas, examine pastoral candidates for orthodoxy, and staff the disputations that police the boundaries. Appointment and promotion run through subscription to the local confession, so movement between confessions forfeits chair, salary, and scholarly network.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, university_theology_faculties, beneficiary,
    organized, generational, constrained, continental).

% Parishioners attend the worship their ruler's confession mandates, pay tithe to its clergy, send children to its catechism, and in wartime furnish sons to armies fighting over confessional boundaries. The same structure provides baptism, preaching, poor relief, and burial, so the household's spiritual and welfare needs are met only through the confession it is born into; relocating to another prince's territory means abandoning land, guild rights, and kin.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ordinary_parish_laity, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, ordinary_parish_laity, beneficiary).

% Anabaptist, Spiritualist, and Socinian congregations hold believer's baptism or anti-trinitarian teaching that every territorial confession condemns. They are fined, imprisoned, executed, or expelled by Catholic, Lutheran, and Reformed authorities alike — Felix Manz drowned in Zurich, Michael Servetus burned in Geneva — and survive by migrating to tolerant enclaves in Moravia, Poland-Lithuania, and later the Atlantic colonies. No confession will host them on equal terms, so refuge rather than membership is their only way out.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religious_dissenters, payer,
    powerless, biographical, constrained, continental).

% Huguenots in France, Catholics in Elizabethan England, Protestants in Habsburg lands live under edicts of toleration that successive governments narrow. They organize militias and political parties where permitted, litigate and petition where not, and pay fines, forfeitures, and recusancy penalties. Their exit is exile, taken at the cost of property and homeland, as the Huguenot diaspora after 1685 would show at scale.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_minorities, payer,
    moderate, biographical, constrained, national).

% Erasmians, Martin Bucer, Georg Cassander, and later unionist writers propose comprehension formulas that would bracket the dividing questions. They are shut out of the settlement conferences or outmaneuvered there — Marburg fails on the Eucharist, Poissy dissolves — and each side suspects them of secret sympathy with the other, costing them preferment and safety in equal measure.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, irenical_moderates, excluded,
    moderate, biographical, constrained, continental).

% Scholars reconstruct the confessional era from visitation records, martyrologies, polemics, and consistory minutes. They hold no confessional stake, can compare all blocs simultaneously, and publish findings that no party's discipline machinery reaches.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within each confession, the confessional documents solve a real coordination problem: which doctrine is normative, who may preach and administer sacraments, how worship and catechesis proceed, and how disputes are adjudicated — settled once, centrally, rather than congregation by congregation.
% TRANSFER_FUNCTION: Moves material support (tithes, fees, endowments, tax-funded church estates) and obedience from laity and dissenters to each confession's clerical and academic establishment; moves membership and legitimacy across confessional lines only at the price of conversion.
% ABSENT_VOICES: Executed and expelled dissenters (Felix Manz, Michael Servetus) are absent by design; radical reformers held no seat at imperial diets, colloquies, or the Westminster Assembly; parish laity were represented only by princely and clerical proxies at Augsburg, Trent, and Westphalia.
% DISAPPEARANCE_RATIONALE: If the confessional boundaries dissolved overnight, the European religious map would reorganize: comprehension unions would gain traction, inter-confessional marriage and office-holding would normalize, the wars of religion would lose their organizing axis, and each confession's revenue and jurisdiction would collapse into whatever settlement absorbed it.
% FOUNDING_PROBLEM: The early-sixteenth-century authority crisis: when conscience reading Scripture collided with received tradition (indulgences, penance, the Eucharist), no agreed procedure existed to settle doctrine. The confessional settlements were built to fix authoritative teaching against both Rome and the radical wing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Joint Declaration on the Doctrine of Justification (1999), signed by the Lutheran World Federation and the Catholic Church and later affirmed by Methodist, Anglican, and Reformed bodies, attests that the founding soteriological dispute no longer divides the signatories; the secular 'confessionalization' school of historiography attests that the settlements consolidated institutional power alongside doctrine. No serious party denies the founding crisis occurred; the contest is over whether it remains live.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon 0.72 at interval end) reflects support decoupled from service — tithe and fee obligations bind regardless of pastoral performance — compounded by the war costs the boundary system made unavoidable. Suppression (0.78) is the load-bearing figure: the boundaries persist through subscription oaths, consistory discipline, heresy statutes, and censorship, not through voluntary assent alone. Theater (0.35) is moderate: the doctrinal content and the catechetical function are real, but a growing share of confessional activity is boundary-performance — formulaic polemic, ritualized anathema, subscription ceremony — rising monotonically across the interval (a Goodhart signal). Accessibility collapse (0.60): alternatives collapse locally under cuius regio but survive at the margins through migration and separatism. Resistance (0.55): dissenting movements, the theological channel of the 1525 rising, the Arminian controversy, recusancy. All three series share one grid (t = 0,20,40,60,80,100,120 mapping to 1530–1650); extraction and suppression crest at t=100 (1630, war peak) and relax partially after Westphalia, while theater never relaxes. Coalition note: the potential coalition of the persecuted is structurally broken — each confession persecutes the radicals, so victims cannot combine across blocs. Claim and metrics are authored independently: tangled_rope is asserted from structure (genuine coordination function plus asymmetric burden plus active enforcement); the engine computes per-seat types from the data.
 *
 * PERSPECTIVAL GAP:
 *   From the denominational_leadership seat the arrangement computes as faithful administration of a trust: the confession preserves the gospel, discipline protects souls, revenue funds the ministry. From the ordinary_parish_laity and religious_dissenters seats the same documents, oaths, and courts compute as a persecution and levy machine. The university seat computes a career structure. Nothing in the artifacts differs across seats; power and exit do all the differentiating — which is why the engine, not the author, must compute the per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows: denominational_leadership sits at the beneficiary extreme (d near 0) — the arrangement subsidizes it, and its identity-lock removes even the temptation of arbitrage; university_theology_faculties sit low-d but constrained (endowed dependence on subscription); ordinary_parish_laity sit near the target extreme (d near 1) as trapped payers who also consume the coordination goods, which damps their effective burden below the pure-target case; religious_dissenters sit at the full-target extreme with the heaviest effective burden in the story — no confession subsidizes them anywhere; confessional_minorities sit high-d with costly-but-real exit (diaspora), placing them just below the dissenters; irenical_moderates carry mid-to-high d through exclusion costs rather than levies. Continental scope raises verification difficulty and thereby amplifies effective burden for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling doctrinal authority once conscience and tradition collided — has contested status: the 1999 Joint Declaration and successor consensus documents indicate the soteriological core no longer divides the major signatories, while continuing confessional division elsewhere keeps parts of the problem live. The classification guards both mislabelings: reading the arrangement as pure predation misses the real coordination it performed (catechesis, liturgy, discipline, and poor relief solved genuine problems for members); reading it as pure coordination misses the suppressed exits and asymmetric burdens. The contested (not dead) founding status paired with a world_rearranges verdict means no zombie flag fires yet, but the theater trajectory and the ecumenical retirements mark this as the leading candidate for post-1650 mandate decay — watch for inertial, performance-maintained dynamics in modern confessional formalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformation_kernel_reading_indexicality,
    'This constraint instantiates the theological_fragmentation_reading of the reformation_composite kernel; would the political_realignment_reading or technological_mediation_reading instantiate a different standing arrangement with a different epsilon?',
    'Compare the sibling stories'' referent arrangements and epsilon values; the kernel''s readings are separate files linked by network.affects_constraints, and divergence between them is expected rather than contradictory.',
    'Classification is reading-indexed: a political-reading instantiation would name princes and imperial estates as principals and likely shift beneficiaries toward dynastic consolidators; epsilon and type could differ across readings without either being wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the reformation_composite kernel.').

omega_variable(
    incompatibility_necessity_ambiguity,
    'Are the denominational boundaries generated by the logical incompatibility of the commitments themselves (persisting regardless of enforcement), or by institutional maintenance riding on genuine but non-forcing disagreement?',
    'Counterfactual analysis of comprehension episodes: Marburg 1529 (Eucharist — failure despite maximal goodwill) versus the Leipzig Interim 1548 (flexibility under coercion) and the Prussian Union 1817 (administrative merger succeeded where doctrine ran parallel).',
    'If a forcing logical core exists, part of the structure behaves like an irreducible limit and enforcement explains less; if not, the entire boundary system is maintained construction and the hybrid coordination-plus-burden reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompatibility_necessity_ambiguity, conceptual, 'Whether confessional incompatibility is logically necessary or institutionally maintained.').

omega_variable(
    fragmentation_rent_attribution,
    'Does denominational leadership benefit from fragmentation as such, or would a unified settlement have delivered comparable revenues to whichever clergy administered it?',
    'Compare clergy income, office density, and endowment growth across fragmented territories versus comprehended or united jurisdictions (post-Union Prussian church economy, the Elizabethan settlement).',
    'If revenues track fragmentation specifically, the beneficiary declaration is load-bearing for the burden assessment; if they track clerical office generally, the burden is generic to establishment and the fragmentation premium is small.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_rent_attribution, empirical, 'Whether leadership gains attach to fragmentation itself or to clerical office generally.').

omega_variable(
    soteriological_dispute_resolution_status,
    'Has the founding soteriological dispute been substantially resolved (Joint Declaration 1999 and successors), rendering the confessional apparatus partially mandate-less?',
    'Track uptake of the Joint Declaration across confessional bodies and whether church-dividing language is retired in official teaching and ordination vows.',
    'Widespread retirement would date the mandate transition and push late-period classifications toward inertial, performance-maintained dynamics; continued division keeps the founding problem live and the enforcement meaningful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soteriological_dispute_resolution_status, empirical, 'Status of the founding soteriological dispute after the ecumenical consensus documents.').

omega_variable(
    confessional_theater_trajectory,
    'Does the rising theater_ratio reflect proxy substitution (polemic and subscription ceremony replacing doctrinal function) or increased performative intensity under existential threat?',
    'Content analysis of visitation records and polemical output: measure the share of confessional activity that is boundary-performance versus catechesis and charitable function across the interval.',
    'Proxy substitution supports inertial-decay hypotheses for the post-1650 apparatus; threat-driven performance would instead predict theater falling once security returns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_theater_trajectory, empirical, 'Driver of the rising performative share of confessional activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__theological_fragmentation_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(refo_tr_t20, observed).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__theological_fragmentation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(refo_tr_t40, observed).
narrative_ontology:measurement(refo_tr_t60, reformation_composite__theological_fragmentation_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(refo_tr_t60, observed).
narrative_ontology:measurement(refo_tr_t80, reformation_composite__theological_fragmentation_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement_basis(refo_tr_t80, observed).
narrative_ontology:measurement(refo_tr_t100, reformation_composite__theological_fragmentation_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(refo_tr_t100, observed).
narrative_ontology:measurement(refo_tr_t120, reformation_composite__theological_fragmentation_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement_basis(refo_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t20, reformation_composite__theological_fragmentation_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(refo_be_t20, observed).
narrative_ontology:measurement(refo_be_t40, reformation_composite__theological_fragmentation_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(refo_be_t40, observed).
narrative_ontology:measurement(refo_be_t60, reformation_composite__theological_fragmentation_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(refo_be_t60, observed).
narrative_ontology:measurement(refo_be_t80, reformation_composite__theological_fragmentation_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement_basis(refo_be_t80, observed).
narrative_ontology:measurement(refo_be_t100, reformation_composite__theological_fragmentation_reading, base_extractiveness, 100, 0.85).
narrative_ontology:measurement_basis(refo_be_t100, observed).
narrative_ontology:measurement(refo_be_t120, reformation_composite__theological_fragmentation_reading, base_extractiveness, 120, 0.72).
narrative_ontology:measurement_basis(refo_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(refo_su_t0, observed).
narrative_ontology:measurement(refo_su_t20, reformation_composite__theological_fragmentation_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(refo_su_t20, observed).
narrative_ontology:measurement(refo_su_t40, reformation_composite__theological_fragmentation_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(refo_su_t40, observed).
narrative_ontology:measurement(refo_su_t60, reformation_composite__theological_fragmentation_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement_basis(refo_su_t60, observed).
narrative_ontology:measurement(refo_su_t80, reformation_composite__theological_fragmentation_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement_basis(refo_su_t80, observed).
narrative_ontology:measurement(refo_su_t100, reformation_composite__theological_fragmentation_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement_basis(refo_su_t100, observed).
narrative_ontology:measurement(refo_su_t120, reformation_composite__theological_fragmentation_reading, suppression_requirement, 120, 0.78).
narrative_ontology:measurement_basis(refo_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, technological_mediation_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three epsilon-invariant readings: theological (this file — referent: the confessional boundary arrangement), political (referent: the sovereignty-differentiation arrangement), and technological (referent: the print-mediated diffusion arrangement). Each carries its own epsilon, beneficiaries, and stakeholders; they are linked as a constraint family rather than merged, because measuring the Reformation through different observables yields different stable epsilon values. The confusion lives in the label 'the Reformation,' not in the history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
