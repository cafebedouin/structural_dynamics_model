% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercive Override of LDS Plural Marriage (Exogenous Override Reading)
 *   domain: religious institutional history / political theology
 *
 * SUMMARY:
 *   Between 1862 and 1904 the United States federal government dismantled the
 *   Latter-day Saint practice of plural marriage by escalating coercion: the
 *   Morrill Anti-Bigamy Act (1862), the Poland Act (1874), the Edmunds Act
 *   (1882), and the Edmunds-Tucker Act (1887) — the last dissolving the
 *   church's corporate charter, escheating its property, disfranchising
 *   Utah's women, and imposing test oaths for office and jury service.
 *   Federal marshals imprisoned hundreds of practitioners; the First
 *   Presidency governed from hiding. In 1890 Woodruff issued the Manifesto
 *   advising members against contracting new plural marriages; Utah statehood
 *   followed in 1896; the Second Manifesto (1904) closed the post-Manifesto
 *   exceptions under Senate pressure. This story instantiates the
 *   exogenous_override_reading of that reversal: the practice ended because
 *   external force made its continuance incompatible with the institution's
 *   survival, not because the doctrine was revised — Section 132 remains
 *   canon, and the standing arrangement is the coercive regime together with
 *   the compliance posture it compelled, a doctrine-practice gap held open
 *   from both sides. The epsilon referent is that standing arrangement,
 *   assessed by this reading's lights. Family note: the kernel decomposes
 *   into three epsilon-invariant readings (this file,
 *   endogenous_reinterpretation_reading, practice_doctrine_gap), linked via
 *   network.affects_constraints; each authors its own epsilon, victims, and
 *   classification. KEY AGENTS (by structural relationship): -
 *   federal_territorial_administration: agenda-setting beneficiary seat
 *   (institutional/arbitrage) — writes and enforces the statutes, collects
 *   surrendered autonomy and escheated property - lds_first_presidency:
 *   primary payer seat (organized/identity_locked) — bears disincorporation
 *   and imprisonment, administers the internal compliance that holds the
 *   doctrine-practice gap - plural_marriage_households: primary payer seat
 *   (powerless/trapped) — households dissolved by prosecution;
 *   covenant-bound, partial exit only via Mexican and Canadian colonies -
 *   utah_federal_appointees: secondary beneficiary seat
 *   (institutional/mobile) — judges, marshals, prosecutors whose offices and
 *   careers ride on enforcement - protestant_moral_reform_lobby: secondary
 *   beneficiary seat (organized/mobile) — national reform organizations whose
 *   program the regime executes - rank_and_file_latter_day_saints: dual
 *   payer/beneficiary seat (moderate/constrained) — disfranchised and raided,
 *   later recipients of statehood's settlement - covenant_objection_minority:
 *   excluded seat (powerless/identity_locked) — holders of Section 132 as
 *   eternal covenant, unseated at the decision point -
 *   constitutional_historians: analytical observer seat — reconstructs the
 *   causal sequence from archives
 *
 * KEY AGENTS:
 *   - federal_territorial_administration: agenda-setting beneficiary seat (institutional/arbitrage) — writes and enforces the statutes, collects surrendered autonomy and escheated property
 *   - lds_first_presidency: primary payer seat (organized/identity_locked) — bears disincorporation and imprisonment, administers the internal compliance that holds the doctrine-practice gap
 *   - plural_marriage_households: primary payer seat (powerless/trapped) — households dissolved by prosecution; covenant-bound, partial exit only via Mexican and Canadian colonies
 *   - utah_federal_appointees: secondary beneficiary seat (institutional/mobile) — judges, marshals, prosecutors whose offices and careers ride on enforcement
 *   - protestant_moral_reform_lobby: secondary beneficiary seat (organized/mobile) — national reform organizations whose program the regime executes
 *   - rank_and_file_latter_day_saints: dual payer/beneficiary seat (moderate/constrained) — disfranchised and raided, later recipients of statehood's settlement
 *   - covenant_objection_minority: excluded seat (powerless/identity_locked) — holders of Section 132 as eternal covenant, unseated at the decision point
 *   - constitutional_historians: analytical observer seat — reconstructs the causal sequence from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.68).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercive Override of LDS Plural Marriage (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious institutional history / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'c2dab1ff-6a05-431b-932d-49264c9e247a').
narrative_ontology:cs_kernel_codification('c2dab1ff-6a05-431b-932d-49264c9e247a', fixed_text).
narrative_ontology:cs_authority_grounding('c2dab1ff-6a05-431b-932d-49264c9e247a', extraction).
narrative_ontology:cs_interpretation_layer_present('c2dab1ff-6a05-431b-932d-49264c9e247a').
narrative_ontology:cs_reading_relation('c2dab1ff-6a05-431b-932d-49264c9e247a', marriage_commitment_reversal__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('c2dab1ff-6a05-431b-932d-49264c9e247a', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('c2dab1ff-6a05-431b-932d-49264c9e247a', foundational, reversal_located_in_external_coercion).
narrative_ontology:cs_axiom_status(reversal_located_in_external_coercion, holdable).
narrative_ontology:cs_axiom_grounding('c2dab1ff-6a05-431b-932d-49264c9e247a', reversal_located_in_external_coercion, empirically_contingent).
narrative_ontology:cs_axiom('c2dab1ff-6a05-431b-932d-49264c9e247a', foundational, section_132_remains_binding_principle).
narrative_ontology:cs_axiom_status(section_132_remains_binding_principle, holdable).
narrative_ontology:cs_axiom_grounding('c2dab1ff-6a05-431b-932d-49264c9e247a', section_132_remains_binding_principle, theological).
narrative_ontology:cs_axiom('c2dab1ff-6a05-431b-932d-49264c9e247a', secondary, manifesto_administrative_not_doctrinal).
narrative_ontology:cs_axiom_status(manifesto_administrative_not_doctrinal, holdable).
narrative_ontology:cs_axiom_grounding('c2dab1ff-6a05-431b-932d-49264c9e247a', manifesto_administrative_not_doctrinal, conventional).
narrative_ontology:cs_reference_frame('c2dab1ff-6a05-431b-932d-49264c9e247a', externally_imposed_practice_suspension).
narrative_ontology:cs_drift_state('c2dab1ff-6a05-431b-932d-49264c9e247a', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2dab1ff-6a05-431b-932d-49264c9e247a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, protestant_moral_reform_lobby).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_households).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_latter_day_saints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, utah_federal_appointees).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_latter_day_saints).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, belief_action_distinction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress and the territorial executive branch pass and enforce the anti-polygamy statutes: appointing federal judges and marshals for Utah, prosecuting unlawful cohabitation, dissolving the church's charter, escheating its property, and conditioning statehood on abandonment of the practice. Collects surrendered autonomy, seized property, and a uniform marriage standard across the territory; can shift among legal, economic, and political instruments as each proves costly.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Leads the church: bears imprisonment, exile, and the dissolution of its corporate existence, then issues the 1890 suspension and administers internal compliance with it thereafter. Its authority rests on the same canon whose practice it must publicly suspend, so abandoning the doctrine outright would dissolve its claim to prophetic continuity; exit would mean renouncing the foundation of its own office.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_first_presidency, agenda_setter).

% Husbands face arrest and prison for unlawful cohabitation; wives lose household providers and legal recognition of their marriages; children inherit stigma. Dissolving the households violates covenants they regard as eternal; continuing them invites prosecution. Some flee to colony settlements in Mexico and Canada at the cost of land, community, and livelihood.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_households, payer,
    powerless, biographical, trapped, regional).

% Territorial judges, marshals, and prosecutors staff the enforcement machinery: salaries, careers, and expanded jurisdiction flow from its operation. Their posts exist because the conflict exists; when enforcement winds down after statehood, the offices and their perquisites shrink.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, utah_federal_appointees, beneficiary,
    institutional, biographical, mobile, regional).

% National Protestant reform organizations campaign for federal suppression of plural marriage as a moral cause; the regime's adoption vindicates their program and delivers its objective. They bear little of its cost and move off the issue once the practice is abandoned.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, protestant_moral_reform_lobby, beneficiary,
    organized, generational, mobile, national).

% Non-practicing members bear disfranchisement (Utah women lose the vote under Edmunds-Tucker regardless of household), test-oath requirements for voting and office, and the disruption of the raids. They also receive the settlement: an end to prosecution, restored normalcy, and eventual statehood. Leaving the church would forfeit their entire community; staying means absorbing both the costs and the gains.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_latter_day_saints, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, rank_and_file_latter_day_saints, beneficiary).

% Members and some leaders who hold Section 132 as an eternal covenant treat the suspension as surrender imposed by force rather than revealed will. They are not seated in the accommodation negotiated between federal authorities and the First Presidency; their objection surfaces later in resignations and eventually in schism, but at the moment of decision they have no vote.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, covenant_objection_minority, excluded,
    powerless, biographical, identity_locked, regional).

% Scholars of religion, law, and the American West reconstruct the causal sequence from archives: legislative records, court proceedings, diaries, and the Woodruff account itself. They attest or dispute where the reversal's cause sits and what the Manifesto was.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_administration).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single civil marriage standard and a single sovereign legal order across federal territory: one definition of lawful marriage, one set of courts, and no corporate religious body exercising parallel governing authority within a territory.
% TRANSFER_FUNCTION: Moves institutional autonomy, corporate property, and jurisdiction over marriage practice from the LDS church to the federal government; moves the liberty of leaders and the integrity of plural households into the federal penal apparatus; moves the vote from Utah women into the federal test-oath regime.
% ABSENT_VOICES: Members bound by Section 132 as eternal covenant, plural wives whose households the suspension dissolves, and the imprisoned or hiding leadership were not seated in the accommodation negotiated between federal authorities and the First Presidency; their objections surface only afterward in resignations and later schism.
% DISAPPEARANCE_RATIONALE: If the federal regime vanished overnight, the church — doctrine intact — would resume contracting plural marriages, Utah's territorial government would revert to church-aligned governance, statehood negotiations would restart from zero, and the national uniform-marriage framework would lose its test case; the settlement architecture of the American West reorganizes around the resumed conflict.
% FOUNDING_PROBLEM: A corporate church exercised quasi-sovereign authority over marriage and politics in a federal territory, operating a marriage system that federal criminal law defined as unlawful — the problem of rival sovereignty and non-uniform marriage law inside U.S. borders.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the federal government's own post-statehood repeal of the regime's operative provisions attests that the problem it was built for had closed; the church's subsequent failure to seek restoration of the practice through legal channels corroborates the same from the payer side; and standard surveys of American religious and western history record the conflict as resolved rather than merely suppressed. The covenant_objection_minority disputes the resolution's legitimacy but attests that the original conflict is gone.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.68 against the standing arrangement under contest — the federal coercion regime together with the compliance posture it compelled — assessed by this reading's lights, in which the reversal was imposed from outside and Section 132 was never revised. The temporal series shows why the scalar sits below the 1890 peak: extraction accumulated through the Edmunds years (0.42 to 0.86) as disincorporation, escheatment, and imprisonment took hold, then partially decayed after statehood converted conquest into settlement (0.71 to 0.68), leaving a permanent cession of marriage-practice jurisdiction. Suppression (0.82) is authored as a raw structural property and is deliberately NOT reconciled to the suppression_requirement series (ending 0.55): the scalar measures the standing coercive force embedded in the arrangement — plural marriage remains criminally prohibited and the Reynolds precedent still forecloses resumption — while the series tracks active enforcement capacity, which spiked through the Raid (0.88 at 1890) and decayed once compliance became self-maintained. That divergence is the story's central finding: coercion was internalized. Theater crosses 0.5 only late (0.51 at 1904) — enforcement was overwhelmingly functional while the conflict was live, and turned ceremonial (congressional scrutiny, public affirmations) once compliance was secured. Resistance is high (0.72) because the regime met twenty-eight years of organized non-compliance: litigation reaching the Supreme Court, concealment of leaders, a church-aligned political party, and press war. Accessibility collapse is 0.66, not higher, because partial exits genuinely existed — underground continuation and the Mexican and Canadian colonies — before the Second Manifesto and border enforcement closed them. All three series share one time grid (seven-year steps) so no metric row is sampled against a substituted scalar. Coalition note: the payer seats did coordinate — plural households and leadership mounted the litigation and political resistance — and the coalition failed against federal superiority, which is why the payer seats' computed classification should rest on their trapped and identity_locked exits rather than on any assumed capacity to organize their way out.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the federal seats compute different types from identical statutory text. From the First Presidency's seat the arrangement is the destruction of a corporate religious sovereignty by force — property seized, charter dissolved, leaders imprisoned, a canon left standing but untouchable. From the territorial administration's seat the same statutes are the ordinary constitutional duty of a republic that does not tolerate rival sovereigns inside its borders. Plural households experience the law as the dissolution of covenants they hold eternal; the reform lobby experiences it as a moral victory won at no cost to itself. The engine computes this divergence from the structural data — power, exit, and role — and the divergence, not any single seat's verdict, is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the federal seats toward the subsidy end: the territorial administration collects surrendered autonomy, escheated property, and a uniform marriage standard (d near 0.0); appointees and the reform lobby collect careers and vindication at low personal cost. Victim declarations drive the church-side seats toward the target end: the First Presidency (payer, identity_locked) and plural households (payer, trapped) sit near full-target because their exits are covenant-blocked rather than merely costly. Rank-and-file saints are genuinely dual-positioned — they paid disfranchisement and raid costs immediately and received statehood's benefits late and diffusely — so the derivation from their paired roles risks averaging them to symmetric; the moderate-atom override (d = 0.58) pins their net position slightly target-side. The excluded covenant_objection_minority is commentary-grade only and drives no classification (R3).
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled rope is the claim that prevents both mislabels. A pure-snare reading erases the regime's real coordination achievement — one marriage law, one sovereign order, the integration that made statehood possible — which is why Congress legislated across four decades rather than simply plundering. A pure-rope reading erases who paid: a dissolved corporation, seized property, imprisoned leaders, disfranchised women, broken households. The structure carries both a genuine coordination function and asymmetric extraction through the same machinery, actively enforced — hence tangled_rope. The R5 interview supplies the obsolescence finding: the founding problem (rival sovereignty over marriage in a territory) is dead — corroborated by the federal government's own post-statehood dismantling of the regime's operative provisions and by the church's failure to seek restoration through legal channels — while the arrangement's residue persists and the world would still rearrange if it vanished. Dead founding problem plus live rearrangement is the capture/zombie signature: the arrangement is now maintained by the doctrine-practice gap it created, administered by the First Presidency in its secondary agenda-setting role, with theater_ratio just past the Goodhart threshold. The mandate has outlived its function; what remains is the gap, held open from both sides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_causal_locus_contest,
    'This constraint is one reading of kernel marriage_commitment_reversal: does the causal locus of the 1890 suspension sit in external coercion (this reading) or in internal revelation (endogenous_reinterpretation_reading)?',
    'Archival adjudication: the documented sequence of escheatment proceedings, statehood conditionality, and Woodruff''s contemporaneous private accounts versus the later canonical narration of the September 23 vision; convergence or divergence between the private record and the public account decides the locus.',
    'If internal revelation is the operative cause, this reading''s epsilon drops toward the sibling''s profile, the victim set contracts toward administrative inconvenience, and the doctrine-practice gap reads as reinterpretation rather than override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_causal_locus_contest, empirical, 'Committer-frame omega: which sibling reading of the marriage_commitment_reversal kernel the causal evidence supports.').

omega_variable(
    section_132_binding_status,
    'Does Section 132 remain a binding principle whose practice is suspended, or has it been effectively superseded in substance while remaining formally canon?',
    'Track official canon status, sealing-policy evolution, and First Presidency statements across the twentieth century; a formal recharacterization or quiet demotion would resolve it.',
    'If superseded in substance, the arrangement decays toward a retired doctrine and the standing extraction is transitional; if binding, the gap is a loaded spring — preserved doctrine with suspended practice — and latent extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_132_binding_status, conceptual, 'Whether the preserved canon is live principle or formal residue.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the standing suppression of the arrangement structural (criminal statute, Reynolds precedent, retained federal instruments) or internalized (self-maintained compliance administered by the First Presidency)?',
    'Counterfactual probe: assess whether practice would resume if the criminal prohibition and constitutional precedent were withdrawn while internal policy held; post-1890 enforcement decay with unchanged compliance indicates internalization.',
    'If internalized, effective suppression exceeds the structural measure and the arrangement persists independent of enforcement — raising piton-adjacent risk for the residue; if structural, removal of the statutes would release the spring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural versus internalized suppression mechanism in the standing arrangement.').

omega_variable(
    coordination_vs_partisan_extraction_mix,
    'How much of the regime''s design and persistence traces to genuine uniform-law coordination versus partisan and economic extraction (platform politics, Gentile land and economic interests, electoral restructuring of Utah)?',
    'Legislative history and voting-pattern analysis of the Morrill, Edmunds, and Edmunds-Tucker coalitions; economic analysis of who gained materially from escheated property and restructured territorial politics.',
    'A dominant partisan-extraction share pushes the computed classification toward snare for most seats; a dominant coordination share stabilizes tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_partisan_extraction_mix, empirical, 'Motivation mix behind the regime''s construction and persistence.').

omega_variable(
    escheat_destination_ambiguity,
    'Did the regime''s gains accrue to the federal state as capturer, or diffuse into public goods — the escheated property funded territorial common schools?',
    'Trace the disposition of escheated assets and the incidence of school funding against who controlled allocation decisions.',
    'If receipt is judged diffuse-public, the receipt surface should be re-authored as diffuse and the capture reading weakens; if allocation control stayed with federal authorities, the named-seat receipt stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escheat_destination_ambiguity, empirical, 'Whether the regime''s gains landed in a capturing seat or dispersed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1862, observed).
narrative_ontology:measurement(marr_tr_t1869, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1869, 0.16).
narrative_ontology:measurement_basis(marr_tr_t1869, observed).
narrative_ontology:measurement(marr_tr_t1876, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1876, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1876, observed).
narrative_ontology:measurement(marr_tr_t1883, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1883, 0.28).
narrative_ontology:measurement_basis(marr_tr_t1883, observed).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.33).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1897, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1897, 0.46).
narrative_ontology:measurement_basis(marr_tr_t1897, observed).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1904, 0.51).
narrative_ontology:measurement_basis(marr_tr_t1904, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.42).
narrative_ontology:measurement_basis(marr_be_t1862, observed).
narrative_ontology:measurement(marr_be_t1869, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1869, 0.46).
narrative_ontology:measurement_basis(marr_be_t1869, observed).
narrative_ontology:measurement(marr_be_t1876, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1876, 0.54).
narrative_ontology:measurement_basis(marr_be_t1876, observed).
narrative_ontology:measurement(marr_be_t1883, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1883, 0.7).
narrative_ontology:measurement_basis(marr_be_t1883, observed).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.86).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1897, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1897, 0.71).
narrative_ontology:measurement_basis(marr_be_t1897, observed).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1904, 0.68).
narrative_ontology:measurement_basis(marr_be_t1904, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement_basis(marr_su_t1862, observed).
narrative_ontology:measurement(marr_su_t1869, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1869, 0.34).
narrative_ontology:measurement_basis(marr_su_t1869, observed).
narrative_ontology:measurement(marr_su_t1876, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1876, 0.48).
narrative_ontology:measurement_basis(marr_su_t1876, observed).
narrative_ontology:measurement(marr_su_t1883, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1883, 0.78).
narrative_ontology:measurement_basis(marr_su_t1883, observed).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1897, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1897, 0.52).
narrative_ontology:measurement_basis(marr_su_t1897, observed).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1904, 0.55).
narrative_ontology:measurement_basis(marr_su_t1904, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% Kernel marriage_commitment_reversal decomposes into three epsilon-invariant readings: this file (exogenous_override_reading — reversal by external coercion, doctrine unrevised, high epsilon against LDS institutional autonomy), endogenous_reinterpretation_reading (reversal by internal revelation — lower epsilon against the church, since the agent of change is internal), and practice_doctrine_gap (the structural ambiguity both produce — principle preserved, practice suspended). The readings are separate constraints with separate victim sets and epsilon values, linked here per the decomposition rule rather than merged. Causal ordering: the exogenous and endogenous readings each generate the gap; the gap reading describes the residue both leave.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
