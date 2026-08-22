% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   In jurisdictions descended from Marbury-style judicial review, an apex
 *   court holds the final word on what the constitution means: its
 *   interpretations bind coordinate branches, lower courts, and subnational
 *   legislatures, and no other institution may overrule them short of
 *   constitutional amendment. The arrangement solves a real settlement
 *   problem — constitutional disputes receive authoritative answers instead
 *   of escalating into interbranch standoffs — while transferring decision
 *   power on the most salient political questions from elected bodies to an
 *   unelected bench staffed through a legal-professional pipeline. This file
 *   instantiates ONE reading of the contested kernel
 *   basic_law_interpretive_authority: the judicial_supremacy_reading, which
 *   holds that final authority properly belongs with independent legal
 *   experts insulated from politics. The sibling readings —
 *   parliamentary_sovereignty_reading and popular_constitutionalism_reading —
 *   are separate constraint files with their own epsilon values,
 *   beneficiary/victim structures, and classifications; they are linked
 *   through network.affects_constraints, not averaged into this one.
 *   Epsilon's referent is the standing arrangement itself (courts hold final
 *   authority), assessed by this reading's own lights: the
 *   counter-majoritarian transfer of decision power is conceded as real and
 *   priced against the settlement and rights-guardianship services the
 *   reading credits to it. KEY AGENTS (by structural relationship): -
 *   apex_constitutional_court: agenda-setter and principal beneficiary
 *   (institutional/identity_locked) — holds and administers the final word -
 *   national_legislature: primary target (powerful/constrained) — statutes
 *   voided, agenda displaced - electoral_majorities: target
 *   (organized/constrained) — preferences overridden between amendments -
 *   state_legislatures: secondary target (moderate/constrained) — most
 *   frequently overridden lawmakers - constitutional_bar_and_legal_academy:
 *   beneficiary (institutional/identity_locked) — staffs and legitimates the
 *   interpretive monopoly - rights_advocacy_litigants: dual-positioned
 *   beneficiary/payer (organized/mobile) - minority_rights_holders:
 *   dual-positioned beneficiary/payer (powerless/trapped) -
 *   departmentalist_branch_officials: excluded (powerful/constrained) — rival
 *   interpretive claim kept out of operative practice -
 *   comparative_constitutional_scholars: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, 'df567dab-2a18-4fb4-8f27-989b3c75d281').
narrative_ontology:cs_kernel_codification('df567dab-2a18-4fb4-8f27-989b3c75d281', fixed_text).
narrative_ontology:cs_authority_grounding('df567dab-2a18-4fb4-8f27-989b3c75d281', expertise).
narrative_ontology:cs_interpretation_layer_present('df567dab-2a18-4fb4-8f27-989b3c75d281').
narrative_ontology:cs_reading_relation('df567dab-2a18-4fb4-8f27-989b3c75d281', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('df567dab-2a18-4fb4-8f27-989b3c75d281', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('df567dab-2a18-4fb4-8f27-989b3c75d281', foundational, final_interpretive_authority_requires_politically_insulated_expertise).
narrative_ontology:cs_axiom_status(final_interpretive_authority_requires_politically_insulated_expertise, holdable).
narrative_ontology:cs_axiom_grounding('df567dab-2a18-4fb4-8f27-989b3c75d281', final_interpretive_authority_requires_politically_insulated_expertise, instrumental).
narrative_ontology:cs_axiom('df567dab-2a18-4fb4-8f27-989b3c75d281', foundational, minority_rights_require_countermajoritarian_guardian).
narrative_ontology:cs_axiom_status(minority_rights_require_countermajoritarian_guardian, holdable).
narrative_ontology:cs_axiom_grounding('df567dab-2a18-4fb4-8f27-989b3c75d281', minority_rights_require_countermajoritarian_guardian, deontological).
narrative_ontology:cs_reference_frame('df567dab-2a18-4fb4-8f27-989b3c75d281', expert_neutral_finality_framework).
narrative_ontology:cs_drift_state('df567dab-2a18-4fb4-8f27-989b3c75d281', contemporary_politicization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('df567dab-2a18-4fb4-8f27-989b3c75d281', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_bar_and_legal_academy).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_holders).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, national_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, state_legislatures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_litigants).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_holders).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, marbury_judicial_review_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_principle).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__judicial_supremacy_reading, neutral_principled_adjudication_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which constitutional disputes to hear, writes the binding interpretation, and its rulings bind every other branch, every lower court, and the states. Collects the final word on constitutional meaning along with the prestige, docket control, and agenda power that come with it. Leaving the role would mean renouncing the institution's own reason for existence; its members are selected for commitment to keeping that role.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court, beneficiary).

% Drafts and passes federal statutes under the standing possibility that the courts will void them, and must shape major legislation around existing doctrine to survive review. When rulings strike its products, the override path runs through supermajorities or constitutional amendment, both rarely reachable. Its agenda migrates toward whatever the courts leave open, and court-curbing proposals carry legitimacy costs that deter pursuit.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, national_legislature, payer,
    powerful, biographical, constrained, national).

% Win elections, enact programs, and then watch portions of those programs decided instead by litigation outcomes they do not control. Their influence on the bench arrives only through appointments that lag elections by years and outlast the coalitions that made them. Coalition capacity exists through parties and amendment campaigns, but amendment has succeeded twenty-seven times in over two centuries against thousands of proposals.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Pass police-power, education, and morality legislation that federal courts review more often than any other category of law; they are the most frequently overridden lawmakers in the system and have the least say in selecting the reviewers.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, state_legislatures, payer,
    moderate, biographical, constrained, regional).

% Supplies the judges, clerks, and doctrinal scholarship through which constitutional meaning is produced, and trains every lawyer inside the assumption that courts finish constitutional arguments. Members' careers, journals, and self-understanding are bound up with expert finality, and few can articulate their professional role without it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_bar_and_legal_academy, beneficiary,
    institutional, generational, identity_locked, national).

% Pursue policy through test cases and impact litigation, winning landmark victories and absorbing losses when the bench turns. They gain a venue that bypasses electoral weakness but pay when rulings go against them, and they retain the option of shifting resources back to electoral and legislative strategy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_litigants, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, rights_advocacy_litigants, payer).

% Depend on courts for protection against hostile majorities and have won durable protections that way; they also bear narrow readings when the bench turns, cannot exit the polity, and can act only through advocacy organizations that themselves depend on litigation access.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_holders, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, minority_rights_holders, payer).

% Presidents, attorneys general, and legislators who hold that each branch reads the Constitution authoritatively for itself and owes obedience only to its own considered judgment. Once the courts claim the final word, acting on this view is treated as crisis conduct, so the position survives as scholarship and occasional rhetoric rather than operative practice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, departmentalist_branch_officials, excluded,
    powerful, biographical, constrained, national).

% Study how different democracies allocate final interpretive authority and document that parliamentary and popular forms also settle constitutional disputes; they take testimony from every seat and hold no stake in any one settlement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative answer to constitutional disputes, converting potential interbranch crises and interstate divergence into adjudicable cases; produces uniform constitutional meaning across a continental jurisdiction and across generations of officials.
% TRANSFER_FUNCTION: Moves final decision power on constitutional questions from elected legislatures and electoral majorities to an unelected bench, and moves practical agenda-setting power toward whoever can bring suit — repeat players with resources.
% ABSENT_VOICES: Departmentalist officials hold a rival account of interpretive authority but sit outside the settlement conversation once courts claim finality; popular assemblies and movement participants who would resolve constitutional questions politically appear only as litigants or subjects of rulings; ordinary citizens encounter the arrangement as compliance, not participation.
% DISAPPEARANCE_RATIONALE: Every branch currently organizes its conduct around the expectation that courts will settle constitutional questions: agencies build programs to survive review, legislators draft to doctrine, executives implement rulings they oppose. Overnight removal would reopen every settled controversy at once — interbranch interpretive standoffs, divergent lower-court regimes, and rapid reconstruction around either legislative finality or departmentalist coequality.
% FOUNDING_PROBLEM: Under the early republic, coordinate branches each claimed equal competence to read the Constitution, and contested laws produced standoffs rather than resolutions; the framers left the allocation of interpretive finality unresolved, and recurring crises demanded a settlement mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and political scientists outside the judiciary document the founding-era settlement failure and the deliberate ambiguity left at the Philadelphia Convention; comparative scholars corroborate that parliamentary-supremacy systems solve the same settlement problem without judicial finality. The bench itself also attests the problem is live, but the corroboration that matters comes from these outside seats; no source outside the benefiting parties attests that this particular settlement form is uniquely necessary.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects a substantial but credited transfer: the counter-majoritarian movement of decision power is conceded even by this reading's own lights, while the settlement and rights-guardianship services are priced against it. Suppression (0.72) is authored as a raw structural property — it is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled — and measures how thoroughly rival interpretive arrangements (departmentalism, popular final authority) are closed off: amendment is nearly unreachable, court-curbing is normatively foreclosed, and professional ideology renders alternatives unthinkable to trained lawyers. Theater (0.46) tracks the growing share of opinion-writing and ethics activity that performs neutrality rather than produces it — reasoned elaboration giving way to unexplained emergency orders. Accessibility collapse (0.52) is moderate: alternatives persist as live positions in other polities and in academic literature but are inoperative inside the system. Resistance (0.62) is sustained: court-curbing bills, jurisdiction-stripping proposals, packing campaigns, and cross-ideological scholarly attack recur every generation. The 1937 dip in the extractiveness series is an external shock (the packing crisis forcing doctrinal retreat), not an oscillation mechanism; the series is otherwise accumulative, which feeds the extraction-accumulation abductive trigger. All three tracked series run on one shared eight-point grid so every metric is authored at every examined time point; the suppression series is included because the story specifically traces enforcement-capacity growth — from voluntary compliance in the early republic to institutionalized, professionally gated compliance today.
 *
 * PERSPECTIVAL GAP:
 *   From the bench's seat the arrangement is the neutral-expert ideal it embodies; from the legislature's seat it is a standing veto over its products wielded by unaccountable lawyers; from the bar's seat it is the professional order that constitutes legal careers; from departmentalist officials it is a usurpation maintained by habit. Same-level divergence: national and state legislatures hold formally similar positions, but federal courts strike subnational laws at roughly two orders of magnitude the rate of federal statutes, so the state seat experiences the arrangement as routine nullification while the national seat experiences episodic confrontation — same nominal power level, radically different exit value. Identity-lock dynamics: the bar's exit is identity_locked — professional self-concept is constituted through expert finality, so even lawyers who criticize particular rulings cannot occupy the departmentalist position without dissolving their professional identity; the bench's lock is institutional (the organization has become its function). If legal education ever decoupled from the finality assumption, both locked seats would reclassify toward constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   The bench sits nearest the beneficiary pole: it administers the arrangement and collects the good it distributes (d near 0.0). The bar shares that pole with slightly higher d — it collects professional rents but also bears discipline from the bench. Rights-advocacy litigants and minority rights holders sit low-to-mid: genuine subsidy through rights protection, partial payment through adverse rulings and dependence on litigation access. The national legislature and electoral majorities sit near the target pole: they fund the settlement with their statutes and their defeated preferences, and their exits (amendment, court-curbing) are prohibitively costly. State legislatures sit furthest toward full target: maximal exposure, minimal selection power over the reviewers. Scope amplification falls hardest on the national seats — verification of 'neutral principle' across a continental jurisdiction is hardest exactly where the stakes are largest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling constitutional standoffs among coordinate branches — is perennial in form (new disagreements never stop arising) yet contested in substance (critics argue the arrangement now manufactures the disputes it resolves, converting political questions into justiciable ones). Status is therefore contested rather than dead: declaring it dead would mislabel a functioning settlement mechanism as zombie administration, while declaring it simply live launders the counter-majoritarian transfer as inevitable cost. The tangled-rope classification holds both truths apart: the coordination function is real and would be missed overnight, and the extraction is real and is borne by identifiable seats. Mandatrophy risk concentrates in the opposite direction — if legitimacy decays past the point where rulings command compliance, the arrangement degrades toward theatrical maintenance (opinions nobody obeys), which the theater_ratio series is positioned to detect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel basic_law_interpretive_authority (reading: judicial_supremacy_reading). What structural changes would the sibling readings — parliamentary_sovereignty_reading and popular_constitutionalism_reading — produce if adopted?',
    'Adoption analysis across the constraint family: under parliamentary sovereignty the judiciary flips from agenda-setting beneficiary to excluded claimant and the legislature from payer to agenda-setter; under popular constitutionalism the terminal-adjudication seat dissolves entirely and the extraction ledger re-describes as displacement of democratic capacity. Each sibling is authored as its own constraint file; compare per-seat classifications across the family rather than hedging within this one.',
    'Classification is reading-relative: the same constitutional text supports a tangled-rope profile under this reading, a different beneficiary/victim structure under parliamentary sovereignty, and no terminal seat at all under popular constitutionalism. Cross-file comparison, not within-story averaging, is the correct instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    guardianship_vs_rent_extraction,
    'Does judicial review, on net, protect discrete and insular minorities, or does it impose legal-class policy preferences that electoral processes would otherwise resolve?',
    'Cross-jurisdictional natural experiment: compare minority-protective outcomes under strong judicial review versus parliamentary-supremacy systems with statutory bills of rights (UK Human Rights Act, New Zealand BORA, Canadian notwithstanding-clause usage), matched on baseline rights profiles.',
    'If protective, a large share of measured extraction is the price of the rights-guardianship service and the profile sits nearer pure coordination; if rent-extractive, the coordination story thins and the profile shifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardianship_vs_rent_extraction, empirical, 'Whether the counter-majoritarian transfer purchases rights protection or class policy preference.').

omega_variable(
    suppression_internalization_degree,
    'How much of the measured suppression is structural (amendment difficulty, appointment lag, jurisdictional design) versus internalized (professional ideology that renders departmentalism and popular final authority literally unthinkable to trained lawyers)?',
    'Compare operative branch-level interpretive offices (attorney-general opinion shops) and polities where departmentalism survives as practice; if alternatives operate without collapse where the professional ideology is weaker, the internalized share is large.',
    'If suppression is substantially internalized, it persists after structural barriers fall and the bar seat stays identity-locked; classification consequences concentrate on the professional seats rather than the structural ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_degree, conceptual, 'Structural versus internalized share of the arrangement''s suppressive force.').

omega_variable(
    legitimacy_enforcement_ratchet,
    'Does declining public legitimacy of the apex court produce an enforcement ratchet (aggressive docket management, unexplained emergency orders, institutionalized compliance demands) or eventual enforcement decay and normalization of branch-level non-compliance?',
    'Track compliance latency and defiance episodes against approval series across successive terms; code emergency-docket expansion as the ratchet indicator.',
    'A ratchet drives suppression higher and accelerates drift toward pure extraction; decay would date a transition toward inertial-theatrical maintenance (opinions nobody enforces).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_enforcement_ratchet, empirical, 'Direction of the legitimacy-enforcement feedback loop.').

omega_variable(
    gridlock_cost_attribution,
    'Are the gridlock costs borne by the legislative process attributable to judicial supremacy (anticipatory drafting-around doctrine, litigation displacement of the agenda) or to separate veto-point structures that would persist under any interpretive settlement?',
    'Compare legislative throughput and agenda composition in judicial-supremacy versus parliamentary-sovereignty polities matched on veto-point density.',
    'If attributable, the legislature''s victim declaration strengthens and effective extraction on that seat rises; if not, the gridlock component drops out of the extraction ledger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_attribution, empirical, 'Attribution of legislative gridlock costs to the interpretive settlement itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1803, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1803, 0.18).
narrative_ontology:measurement_basis(basi_tr_t1803, observed).
narrative_ontology:measurement(basi_tr_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1857, 0.24).
narrative_ontology:measurement_basis(basi_tr_t1857, observed).
narrative_ontology:measurement(basi_tr_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1905, 0.3).
narrative_ontology:measurement_basis(basi_tr_t1905, observed).
narrative_ontology:measurement(basi_tr_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1937, 0.34).
narrative_ontology:measurement_basis(basi_tr_t1937, observed).
narrative_ontology:measurement(basi_tr_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1954, 0.31).
narrative_ontology:measurement_basis(basi_tr_t1954, observed).
narrative_ontology:measurement(basi_tr_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1973, 0.37).
narrative_ontology:measurement_basis(basi_tr_t1973, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2026, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2026, 0.46).
narrative_ontology:measurement_basis(basi_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1803, 0.27).
narrative_ontology:measurement_basis(basi_be_t1803, observed).
narrative_ontology:measurement(basi_be_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1857, 0.42).
narrative_ontology:measurement_basis(basi_be_t1857, observed).
narrative_ontology:measurement(basi_be_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1905, 0.55).
narrative_ontology:measurement_basis(basi_be_t1905, observed).
narrative_ontology:measurement(basi_be_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement_basis(basi_be_t1937, observed).
narrative_ontology:measurement(basi_be_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1954, 0.52).
narrative_ontology:measurement_basis(basi_be_t1954, observed).
narrative_ontology:measurement(basi_be_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1973, 0.59).
narrative_ontology:measurement_basis(basi_be_t1973, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2026, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(basi_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1803, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1803, 0.34).
narrative_ontology:measurement_basis(basi_su_t1803, observed).
narrative_ontology:measurement(basi_su_t1857, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1857, 0.4).
narrative_ontology:measurement_basis(basi_su_t1857, observed).
narrative_ontology:measurement(basi_su_t1905, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1905, 0.47).
narrative_ontology:measurement_basis(basi_su_t1905, observed).
narrative_ontology:measurement(basi_su_t1937, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1937, 0.54).
narrative_ontology:measurement_basis(basi_su_t1937, observed).
narrative_ontology:measurement(basi_su_t1954, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1954, 0.61).
narrative_ontology:measurement_basis(basi_su_t1954, observed).
narrative_ontology:measurement(basi_su_t1973, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1973, 0.65).
narrative_ontology:measurement_basis(basi_su_t1973, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2026, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(basi_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who interprets the constitution' decomposes into three structurally distinct constraints — one per reading of the kernel basic_law_interpretive_authority. This member (judicial supremacy) carries epsilon 0.62: the counter-majoritarian transfer conceded and priced against credited settlement services. The parliamentary_sovereignty_reading relocates the beneficiary set to the legislature and re-describes judicial review as usurpation; the popular_constitutionalism_reading dissolves the terminal seat entirely and measures extraction as displacement of democratic capacity itself. Upstream/downstream: every reading accepts that disputes need resolution (the settlement-function consensus is upstream of all three), and this reading is upstream of concrete jurisprudential constraints that inherit its victim structure. All family members link through affects_constraints; per-seat classifications are compared across files, never merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
