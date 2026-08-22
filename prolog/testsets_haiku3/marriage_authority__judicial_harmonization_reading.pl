% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Constitutional Floor via Judicial Harmonization of Marriage Authority
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint describes one institutional pathway for evolving marriage
 *   authority within legal pluralism: case-by-case Supreme Court review that
 *   imposes a constitutional floor across all personal law systems without
 *   formal legislative codification (Uniform Civil Code). Individual
 *   litigants from religious communities bring cases challenging personal law
 *   norms on constitutional grounds. The Court accepts jurisdiction and
 *   issues holdings that bind the states and the communities, establishing
 *   enforceable constitutional minima (gender equality, procedural fairness,
 *   minimum age, consent). Communities retain authority above the floor, but
 *   the floor expands incrementally with each ruling. This is neither full
 *   pluralism (community authority is genuinely constrained) nor full
 *   codification (legislation did not act; the code remains jurisprudential
 *   and judicially revisable). It is a scaffold mechanism: a transitional
 *   institutional pathway solving the coordination problem 'how to protect
 *   rights within pluralism' by having courts do the harmonization work that
 *   legislatures could do but have not. The mechanism is extractive: it
 *   concentrates decision-making authority in state courts, expands judicial
 *   power, and derives legitimacy from the constraint's own operation. It is
 *   also suppressive: communities can resist but cannot exit; individuals
 *   face retaliation after judicial victories; the mechanism persists through
 *   enforcement, not consent. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as a scaffold (transitional, solving a founding
 *   problem) while the authored metrics show growing extractiveness and
 *   theater-ratio increase over the interval—the metrics capture the actual
 *   operation (judicial authority expansion) rather than the normative
 *   framing (rights protection within pluralism). The engine measures this
 *   divergence.
 *
 * KEY AGENTS:
 *   - supreme_court_judiciary: Institutional agenda-setter. Sets constitutional floor via landmark rulings. Derives authority and expansion from constitutional mandate. Beneficiary of judicial review authority vindication.
 *   - religious_community_autonomy: Organized payer. Formerly autonomous in personal law matters. Now subject to judicial review on any case. Constrained exit—cannot prevent court jurisdiction or ignore constitutional holdings.
 *   - constitutional_equality_advocates: Powerful beneficiary. Mobilize litigation strategy. Do not execute law but shape its scope via case selection. Partner with judiciary in framing.
 *   - religious_communities_litigants: Powerless, trapped. Used as entry point for judicial intervention. Face community retaliation after winning constitutional holdings.
 *   - legislative_secularists: Powerful but excluded from legislative control. Incidental beneficiary from judicial constraint toward secular standards. Win de facto code convergence without formal codification.
 *   - community_councils_and_courts: Organized payers. Retain authority within floor but subject to judicial review and reversal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.58).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.41).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Constitutional Floor via Judicial Harmonization of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'e91836ec-7e5e-4c50-af6d-70207d9e5348').
narrative_ontology:cs_kernel_codification('e91836ec-7e5e-4c50-af6d-70207d9e5348', fixed_text).
narrative_ontology:cs_authority_grounding('e91836ec-7e5e-4c50-af6d-70207d9e5348', extraction).
narrative_ontology:cs_interpretation_layer_present('e91836ec-7e5e-4c50-af6d-70207d9e5348').
narrative_ontology:cs_reading_relation('e91836ec-7e5e-4c50-af6d-70207d9e5348', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e91836ec-7e5e-4c50-af6d-70207d9e5348', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('e91836ec-7e5e-4c50-af6d-70207d9e5348', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e91836ec-7e5e-4c50-af6d-70207d9e5348', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('e91836ec-7e5e-4c50-af6d-70207d9e5348', foundational, constitutional_review_authority_over_personal_law).
narrative_ontology:cs_axiom_status(constitutional_review_authority_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('e91836ec-7e5e-4c50-af6d-70207d9e5348', constitutional_review_authority_over_personal_law, deontological).
narrative_ontology:cs_axiom('e91836ec-7e5e-4c50-af6d-70207d9e5348', foundational, evolutionary_harmonization_through_litigation).
narrative_ontology:cs_axiom_status(evolutionary_harmonization_through_litigation, holdable).
narrative_ontology:cs_axiom_grounding('e91836ec-7e5e-4c50-af6d-70207d9e5348', evolutionary_harmonization_through_litigation, instrumental).
narrative_ontology:cs_reference_frame('e91836ec-7e5e-4c50-af6d-70207d9e5348', community_autonomous_personal_law_adjudication).
narrative_ontology:cs_drift_state('e91836ec-7e5e-4c50-af6d-70207d9e5348', contemporary_judicial_floor_expansion, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e91836ec-7e5e-4c50-af6d-70207d9e5348', '2026-06-19T14:32:18Z').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_equality_advocates).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, religious_community_autonomy).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, legislative_harmonization_resisters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, legislative_secularists).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, community_councils_and_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues landmark rulings on marriage, divorce, property rights, and procedural access in personal law cases. Sets constitutional floor that all personal law systems must respect while preserving community autonomy above that floor. Accepts individual petitions from within religious communities and uses them to iteratively define constitutional limits. Derives legitimacy and jurisdictional expansion from the constitutional mandate; each ruling vindicates judicial review authority while narrowing community discretion.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Personal law authority historically grounded in community religious tradition and internal governance. Now subject to Supreme Court review on any case a community member brings. Community courts (rabbinical, ecclesiastical, sharia councils) retain authority to regulate affairs within constitutional floor but cannot prevent judicial review or override constitutional holdings. Resistance to floor expansion is met with enforcement: violations trigger contempt, debarment, or dissolution authority.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_community_autonomy, payer,
    organized, civilizational, constrained, national).

% Mobilize litigation strategy targeting vulnerable subgroups within personal law systems (women in marriage dissolution, inter-caste couples, LGBTQ+ individuals seeking recognition). Each litigation win expands the constitutional floor and constrains community discretion. Do not execute law themselves but shape its scope through strategic case selection and legal argument. Gain institutional voice and normative authority as the judiciary partners with their framing.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_equality_advocates, beneficiary,
    powerful, generational, mobile, national).

% Legislative factions (secularist, religious-pluralist) that advocate for either formal Uniform Civil Code or for strengthened community autonomy. Judicial harmonization bypasses them: neither codification nor entrenchment of pluralism results from litigation, yet incrementally the constitutional floor constrains all personal law systems. Legislative initiative is pre-empted by the accumulating constitutional holdings; legislatures become reactive rather than constitutive. Carry the cost of defending existing frameworks against judicial pressure without controlling the outcome.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislative_harmonization_resisters, payer,
    moderate, biographical, constrained, national).

% Individual members bringing cases to Supreme Court: women seeking divorce outside community norms, partners in marriages the community denies recognition, individuals claiming rights the community law denies. Formally in the case, but the constraint operates on them through community enforcement after the ruling (ostracism, loss of standing in community institutions, family rupture). They are used as the entry point for judicial intervention but face retaliation or exit from community support networks once they have won the constitutional holding.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_communities_litigants, excluded,
    powerless, biographical, trapped, local).

% Advocate for elimination of personal law pluralism via formal Uniform Civil Code. Benefit incidentally when the Court imposes constitutional floors that move all systems toward secular standards (gender equality, property rights, procedural fairness). Excluded from control: cannot legislate the code (lack parliamentary supermajority or coalition support) but win de facto convergence through case-by-case constitutional constraint. This is both victory and limitation—the code remains uncodified, subject to future litigation, and reversible by future judicial composition.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislative_secularists, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, legislative_secularists, excluded).

% Traditional adjudicatory bodies (rabbinical courts, church tribunals, sharia councils) that historically resolved marriage disputes within community autonomy. Now subject to judicial review and constitutional constraint. Retain decision-making authority within the constitutional floor but cannot exclude the Supreme Court and must anticipate that rulings they issue can be appealed and overturned. Their legitimacy shifts from community-recognized tradition to state-tolerated domestic jurisdiction—a precarious institutional position.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, community_councils_and_courts, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, community_councils_and_courts, observer).

% Legal scholars, comparative-law practitioners, and constitutional analysts who study whether case-by-case judicial constraint is a legitimate and effective path to family law harmonization. They document the constraint's operation, measure divergences between community and constitutional holdings, and theorize whether the mechanism achieves substantive justice or merely displaces conflict.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_doctrine_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes enforceable baseline equality and procedural fairness in marriage authority without formally eliminating personal law pluralism or imposing unitary civil code. Enables individuals to exit or challenge personal law norms in state courts while preserving community autonomy above the constitutional floor. Solves the structural problem: how to protect fundamental rights within legal pluralism without centralizing law or eliminating community jurisdiction.
% TRANSFER_FUNCTION: Transfers decision-making authority from community courts to state courts on any case a litigant brings, expanding the scope of justiciability in personal law matters. Transfers legitimacy from community tradition to constitutional doctrine. Transfers the cost of defending personal law norms from communities (who controlled the discourse) to legislatures and traditional institutions (who now must justify exceptions to constitutional principles).
% ABSENT_VOICES: Individuals trapped within personal law systems who lack access to or knowledge of Supreme Court review; illiterate, economically dependent, or socially isolated community members cannot litigate (even if willing to bear community retaliation). Community voices opposed to judicial review—fundamentalist factions, traditionalist councils—are present but progressively outweighed by constitutional precedent, not included in the final consensus.
% DISAPPEARANCE_RATIONALE: If judicial review of personal law ceased tomorrow, community courts would restore unappealed authority; individuals could no longer exit to state courts; constitutional floors would collapse where communities choose to ignore them; gender equality and procedural fairness protections would evaporate in jurisdictions where community norms diverge. The entire architecture of rights-protection-within-pluralism would dissolve.
% FOUNDING_PROBLEM: Legal pluralism coexisted with systematic subordination of women and minorities within personal law systems. Communities wielded law as a tool of internal control; individuals had no exit short of apostasy or legal dissolution of community membership. How to protect rights within pluralism without demolishing pluralism itself?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional equality advocates, women's rights organizations, and comparative legal scholars outside the benefiting institutions attest that gender-based exclusion and procedural unfairness remain endemic to personal law systems. Community autonomy advocates and religious leaders contest the claim that a 'founding problem' exists; they frame the constraint as externally imposed jurisdiction creep, not solution to a problem they recognize. Independent empirical studies document persistent gaps between constitutional guarantees and community practice—corroborating the live status from outside beneficiary seats.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as the degree to which the constraint transfers decision-making authority from communities to courts and the degree to which the constraint's persistence depends on enforcement rather than consent. At interval start (1975), the constraint is nascent: courts rarely intervene in personal law, communities retain substantial autonomy. Extractiveness is moderate (0.28) because the mechanism is still establishing itself. Over the interval, landmark rulings expand the floor: gender equality, procedural fairness, minimum age, consent requirements accumulate. Each ruling constrains community discretion further. By 2025, extractiveness has grown to 0.58 because the constraint now systematically converts personal law disputes into constitutional cases, transferring authority from communities to courts. Suppression grows more slowly (0.18 to 0.41) because the mechanism does not rely on raw coercion but on judicial authority and the diffuse enforcement of constitutional holdings through contempt, debarment, and institutional pressure. Theater-ratio rises from 0.08 to 0.32 because communities are forced to justify personal law norms as 'compatible with constitutional principles' even when the norms are fundamentally incompatible—the constraint requires ritual compliance performance (community courts issuing decisions that 'follow' constitutional holdings while maintaining cultural legitimacy) that increases over time as the gap between tradition and constitutional doctrine widens. The coercion_grid shows the key dynamics: at the individual level (1975), accessibility_collapse is high (0.85) because individuals in personal law systems lack real exit options; by 2025, this falls to 0.52 as judicial review provides an exit route, collapsing the individual-level suppression that previously kept people in community systems. However, at the organizational level (communities), suppression rises from 0.22 to 0.35 as judicial constraint tightens and communities face escalating enforcement pressure. Resistance at individual and organizational levels both rise over the interval (individual: 0.12→0.48, organizational: 0.58→0.72) as the constraint becomes more visible and oppressive. The scaffold claim is appropriate because the founding problem (rights protection within pluralism) is live and the constraint solves it; however, the measurement series show that the mechanism is accumulating extraction and theater as it operates—classic scaffold drift toward snare if the constraint persists past the founding problem's resolution or if the judicial machinery is captured.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court sees this constraint as legitimate judicial review vindicating constitutional supremacy—the court's seat computes it as rope (coordination around constitutional principles). Religious communities see it as illegitimate external jurisdiction creep into their autonomous domain—their seat computes it as snare (pure extraction of authority, no genuine coordination). Constitutional equality advocates see it as the necessary mechanism for protecting rights in pluralism—their seat computes it as rope or tangled_rope (coordination with asymmetric extraction aimed at justice). Legislative secularists see it as a second-best path to UCC goals but lack control—their seat would compute it as scaffold (transitional toward their preferred endpoint) but they have been excluded from legislative initiative. The governance asymmetry: the Court controls the pace and scope of constitutional floor expansion; communities can only react; legislatures are pre-empted; litigants are used as entry points but excluded from closure. These structural asymmetries should produce markedly different type classifications across seats—that divergence is the signal the apparatus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The supreme_court_judiciary is the clear beneficiary: d approaches 0.0 (the constraint expands judicial authority, vindicates judicial review, and concentrates decision-making in the judicial seat). Religious_community_autonomy is the target: d approaches 1.0 (the constraint systematically constrains their authority and forces defensive compliance). Constitutional_equality_advocates are beneficiaries at d ~ 0.1 because the constraint implements their preferred outcomes without them executing law—they benefit from expansion of rights protections. Religious_communities_litigants are deeply trapped targets (d ~ 0.95): they gain the right to petition courts but face community retaliation; the constraint creates exit formally but suppresses exit materially. Legislative_harmonization_resisters are payers (d ~ 0.75) because they lose legislative initiative to the accumulating constitutional floor. Directionality derivation is straightforward from the structural data: beneficiaries (court, advocates) derive d from the power/exit profile of institutional power + analytical/mobile exit = low d; victims (communities, litigants under suppression) derive d from organized/powerful power + constrained exit (for communities) = high d. No overrides are necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical scaffold: it was built to solve a live founding problem (how to protect rights within pluralism without eliminating pluralism). The founding problem remains live (gender-based exclusion, procedural unfairness, and minority subordination continue in personal law systems). However, the measurement series reveal classic scaffold drift dynamics. The constraint's mechanism—judicial imposition of constitutional floors—is extractive: it concentrates authority in courts, expands judicial power, and derives legitimacy from vindicating judicial review. The theater-ratio rise (0.08→0.32) signals that communities are no longer genuinely coordinating around constitutional principles but performing compliance. The suppression_requirement rise (0.18→0.41) signals that the constraint's persistence depends increasingly on enforcement (contempt, institutional pressure, debarment) rather than on voluntary adoption. These dynamics create a risk of mandatrophy (functional inversion): the constraint was meant to transition toward a settled state where communities internalize constitutional values and voluntarily comply with floors. Instead, it is becoming a permanent extraction mechanism because neither legislatures codified a formal UCC (which would lock the floor and shift burden from courts) nor communities adopted constitutional values voluntarily (which would make judicial enforcement unnecessary). The scaffold is at risk of becoming a snare if the founding problem is declared 'solved' while the mechanism persists unchanged. The measurement data support this reading: extractiveness and theater are both rising, not falling—the characteristic trajectory of a mechanism that has moved past its sunset date but continues to operate. An exit test: if the Supreme Court declared that the constitutional floor was now settled and courts would defer to communities on any question above the floor, would communities voluntarily maintain the floor or drift below it? The evidence suggests drift: communities have consistently challenged floor rulings and resisted change. This means the floor persists because of judicial enforcement, not because communities have internalized the principle. Mandatrophy diagnosis: live (the constraint persists after solving its founding problem, purely through enforcement and theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_consent_vs_coercion,
    'Is the rising suppression_requirement driven by communities gradually internalizing constitutional values (moving toward voluntary compliance), or by communities increasingly resisting the floor (requiring escalating enforcement)?',
    'Post-judicial-withdrawal experiment: if appellate courts in a jurisdiction temporarily ceased reviewing personal law cases for 5 years, would communities maintain constitutional floors or drift below them? Attrition of community institutions (courts, councils) would indicate coercion; reform of community law would indicate internalization.',
    'If coercive: the constraint is a snare wearing a scaffold''s clothes; the founding problem solution is illusory because suppression must persist indefinitely. If consensual: the constraint is genuinely transitional and could sunset as communities internalize floors. The classification would shift from scaffold+extractive to tangled_rope (if coercive) or rope (if consensual).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_consent_vs_coercion, empirical, 'Whether suppression masks genuine consensus or reveals persistent resistance.').

omega_variable(
    alternative_harmonization_pathways,
    'Could legislative codification (formal UCC) or community-led reform (bottom-up constitutional incorporation) achieve the same floor without judicial extraction? Is judicial pathway necessary or merely convenient for the Court?',
    'Comparative analysis of jurisdictions with different pathways: legislatively codified vs. judicially harmonized vs. community-led reform. Measure extractiveness, suppression, community consent, and rights protection in each.',
    'If alternative pathways are equally effective and less extractive, the judicial pathway is structurally suboptimal and captures its own justification (vindicating judicial review). If alternative pathways fail or communities refuse reform, judicial pathway is functionally necessary despite extractiveness. This informs whether the constraint is a legitimate scaffold or a captured mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_harmonization_pathways, empirical, 'Whether judicial harmonization is the only viable path to rights protection in pluralism.').

omega_variable(
    reading_framework_underspecification,
    'Is ''judicial harmonization'' a sufficient framing for this constraint, or does it conflate institutional mechanism with normative reading? Could this constraint be instantiated by courts implementing communal_autonomy_reading (courts review only procedural fairness, not substantive norms) or gender_rights_reading (courts prioritize gender equality above all)?',
    'Examine Court holdings across decades: do they apply a consistent normative reading (equality, procedural fairness, secular standards) or do they shift with judicial composition? If consistent normative reading, the constraint is inseparable from that reading; if shifting, judicial harmonization is mechanism-neutral and is paired with different readings at different times.',
    'If mechanism-neutral: this constraint should decompose into mechanistic (who decides) and normative (what counts as constitutional) components, producing a constraint family. If mechanism-bound to normative reading: this constraint is one reading, not a mechanism, and the ε_mod should reflect the particular normative content (secular standards, gender equality) not the abstraction of ''harmonization.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framework_underspecification, conceptual, 'Whether judicial harmonization is a framework choice or a commitment to specific normative principles.').

omega_variable(
    literature_canonical_ambiguity,
    'The source material labels this reading ''institutional mechanism rather than distinct normative reading'' and ''ε_mod scaffold with judiciary as beneficiary.'' Is the reading''s distinctiveness structural (focus on courts as mechanism) or value-instantiated (focus on convergence outcomes)? Could the secularist reading, communal_autonomy reading, and gender_rights reading all instantiate the same judicial harmonization mechanism?',
    'Deconstruct the source-material ε_mod: does ''judiciary as beneficiary'' create the ε value, or do the substantive holdings (gender equality, secular standards, procedural fairness) create it? If the former, the reading is about mechanism; if the latter, it is about outcomes and should be paired with a normative reading.',
    'If mechanism-as-reading: the constraint remains as specified, focused on institutional pathway. If outcome-as-reading: this constraint should be recast as one instance of a family where different readings (gender_rights, secularist, communal_autonomy) all flow through judicial mechanism, producing a sibling-structure change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literature_canonical_ambiguity, conceptual, 'Kernel reading specification ambiguity: institutional mechanism vs. normative content.').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of community resistance structural (communities are legally barred from appealing or ignoring rulings) or internalized (communities believe they should comply even if they fundamentally disagree)?',
    'Measure post-exit suppression: if a community member leaves the community after winning a Supreme Court case, does suppression persist in their social environment? If yes, suppression is partially internalized (the individual has internalized the community''s internalization of subordination, a double bind). If no, suppression is purely structural.',
    'If internalized: the constraint carries suppression into the next generation and dissolves family/community bonds; it is deeply extractive. If structural: the constraint''s burden is transactional (compliance with unfavorable rules) rather than identity-destructive. Theater-ratio interpretation shifts: rising theater could signal ritualized resistance (communities performing compliance while teaching resistance) vs. identity collapse (individuals performing constitutionalism while grieving the loss of community identity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in judicial constraint of community authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1975, marriage_authority__judicial_harmonization_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__judicial_harmonization_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority__judicial_harmonization_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__judicial_harmonization_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority__judicial_harmonization_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__judicial_harmonization_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(marr_be_t1975, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(marr_be_t1995, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(marr_be_t2015, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1975, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1975, 0.18).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1985, 0.24).
narrative_ontology:measurement(marr_su_t1995, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1995, 0.31).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(marr_su_t2015, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2015, 0.39).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2025, 0.41).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1975, tn=2025
narrative_ontology:measurement(marr_grid_01, marriage_authority__judicial_harmonization_reading, accessibility_collapse(class), 1975, 0.68).
narrative_ontology:measurement(marr_grid_02, marriage_authority__judicial_harmonization_reading, accessibility_collapse(class), 2025, 0.44).
narrative_ontology:measurement(marr_grid_03, marriage_authority__judicial_harmonization_reading, accessibility_collapse(individual), 1975, 0.85).
narrative_ontology:measurement(marr_grid_04, marriage_authority__judicial_harmonization_reading, accessibility_collapse(individual), 2025, 0.52).
narrative_ontology:measurement(marr_grid_05, marriage_authority__judicial_harmonization_reading, accessibility_collapse(organizational), 1975, 0.72).
narrative_ontology:measurement(marr_grid_06, marriage_authority__judicial_harmonization_reading, accessibility_collapse(organizational), 2025, 0.48).
narrative_ontology:measurement(marr_grid_07, marriage_authority__judicial_harmonization_reading, accessibility_collapse(structural), 1975, 0.42).
narrative_ontology:measurement(marr_grid_08, marriage_authority__judicial_harmonization_reading, accessibility_collapse(structural), 2025, 0.38).
narrative_ontology:measurement(marr_grid_09, marriage_authority__judicial_harmonization_reading, resistance(class), 1975, 0.35).
narrative_ontology:measurement(marr_grid_10, marriage_authority__judicial_harmonization_reading, resistance(class), 2025, 0.68).
narrative_ontology:measurement(marr_grid_11, marriage_authority__judicial_harmonization_reading, resistance(individual), 1975, 0.12).
narrative_ontology:measurement(marr_grid_12, marriage_authority__judicial_harmonization_reading, resistance(individual), 2025, 0.48).
narrative_ontology:measurement(marr_grid_13, marriage_authority__judicial_harmonization_reading, resistance(organizational), 1975, 0.58).
narrative_ontology:measurement(marr_grid_14, marriage_authority__judicial_harmonization_reading, resistance(organizational), 2025, 0.72).
narrative_ontology:measurement(marr_grid_15, marriage_authority__judicial_harmonization_reading, resistance(structural), 1975, 0.28).
narrative_ontology:measurement(marr_grid_16, marriage_authority__judicial_harmonization_reading, resistance(structural), 2025, 0.61).
narrative_ontology:measurement(marr_grid_17, marriage_authority__judicial_harmonization_reading, stakes_inflation(class), 1975, 0.51).
narrative_ontology:measurement(marr_grid_18, marriage_authority__judicial_harmonization_reading, stakes_inflation(class), 2025, 0.62).
narrative_ontology:measurement(marr_grid_19, marriage_authority__judicial_harmonization_reading, stakes_inflation(individual), 1975, 0.62).
narrative_ontology:measurement(marr_grid_20, marriage_authority__judicial_harmonization_reading, stakes_inflation(individual), 2025, 0.71).
narrative_ontology:measurement(marr_grid_21, marriage_authority__judicial_harmonization_reading, stakes_inflation(organizational), 1975, 0.48).
narrative_ontology:measurement(marr_grid_22, marriage_authority__judicial_harmonization_reading, stakes_inflation(organizational), 2025, 0.59).
narrative_ontology:measurement(marr_grid_23, marriage_authority__judicial_harmonization_reading, stakes_inflation(structural), 1975, 0.38).
narrative_ontology:measurement(marr_grid_24, marriage_authority__judicial_harmonization_reading, stakes_inflation(structural), 2025, 0.45).
narrative_ontology:measurement(marr_grid_25, marriage_authority__judicial_harmonization_reading, suppression(class), 1975, 0.18).
narrative_ontology:measurement(marr_grid_26, marriage_authority__judicial_harmonization_reading, suppression(class), 2025, 0.32).
narrative_ontology:measurement(marr_grid_27, marriage_authority__judicial_harmonization_reading, suppression(individual), 1975, 0.76).
narrative_ontology:measurement(marr_grid_28, marriage_authority__judicial_harmonization_reading, suppression(individual), 2025, 0.61).
narrative_ontology:measurement(marr_grid_29, marriage_authority__judicial_harmonization_reading, suppression(organizational), 1975, 0.22).
narrative_ontology:measurement(marr_grid_30, marriage_authority__judicial_harmonization_reading, suppression(organizational), 2025, 0.35).
narrative_ontology:measurement(marr_grid_31, marriage_authority__judicial_harmonization_reading, suppression(structural), 1975, 0.12).
narrative_ontology:measurement(marr_grid_32, marriage_authority__judicial_harmonization_reading, suppression(structural), 2025, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__judicial_harmonization_reading, 0.14).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of marriage_authority kernel. It focuses on institutional mechanism (judicial review pathway) for establishing constitutional floor across personal law systems without formal legislative codification. Sibling readings instantiate different normative frameworks (communal autonomy, gender rights, secularism) and different institutional pathways (community authority, gender-equality courts, legislative UCC). The constraint family models the same kernel under contested readings; ε values diverge because the readings assess different aspects of the standing arrangement (mechanism, outcomes, authority legitimacy). All members must be linked via network.affects_constraints to establish family kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
