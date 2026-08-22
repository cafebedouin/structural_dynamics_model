% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Marriage Authority: Shariat-Based Interpretation by Muslim Personal Law Boards and Qazis
 *   domain: legal/constitutional/religious
 *
 * SUMMARY:
 *   In post-independence India, marriage and family law authority under this
 *   reading derives from Shariat as interpreted by Muslim personal law boards
 *   (such as the All India Muslim Personal Law Board, established 1973,
 *   though informal shariat councils predate independence) and qazis (Islamic
 *   judges). The constraint embodies a plural legal order: the Indian
 *   Constitution recognizes minority communal law as legitimate (Article 29,
 *   30), but increasingly subjects it to constitutional review on
 *   gender-equity grounds (Articles 14, 15). This story captures one reading
 *   of the contested kernel 'marriage authority' — specifically, the reading
 *   that anchors authority in Quranic/hadith interpretation through communal
 *   institutions. The reading is both a coordination mechanism (preserving
 *   Islamic legal principles for a religious minority) and an extraction
 *   mechanism (asymmetric power in marriage dissolution, inheritance,
 *   guardianship flows to male household heads; adjudication power
 *   concentrates in scholarly/institutional hands). The claim/metric
 *   divergence is intentional: the constraint is CLAIMED as tangled_rope (it
 *   has genuine coordination function AND asymmetric extraction), while the
 *   authored metrics describe substantial extractiveness (0.68) and active
 *   enforcement (suppression 0.61, requiring_active_enforcement: true) — the
 *   engine measures whether the claim fits the structure.
 *
 * KEY AGENTS:
 *   - Muslim personal law boards: agenda-setters, maintain institutional authority over family law interpretation and adjudication
 *   - Islamic scholars (qazis): derive status and authority from interpretive monopoly, resist state judicial review
 *   - Male household heads: structural beneficiaries of unilateral talaq, legal polygamy, default guardianship
 *   - Muslim women: structural payers, subject to asymmetric divorce/inheritance/guardianship rules
 *   - State judiciary: parallel authority, increasingly asserting constitutional jurisdiction over personal law
 *   - Political majority: excluded from personal law boards, historically pushed for uniform civil code
 *   - Muslim reform movements: excluded from orthodox boards, challenge interpretive monopoly
 *   - Muslim diaspora community: benefits from communal autonomy preservation but contains internal conflicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.61).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Marriage Authority: Shariat-Based Interpretation by Muslim Personal Law Boards and Qazis").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/constitutional/religious").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '8c7f2034-cc22-4151-84ee-058b5325179e').
narrative_ontology:cs_kernel_codification('8c7f2034-cc22-4151-84ee-058b5325179e', fixed_text).
narrative_ontology:cs_authority_grounding('8c7f2034-cc22-4151-84ee-058b5325179e', lineage).
narrative_ontology:cs_interpretation_layer_present('8c7f2034-cc22-4151-84ee-058b5325179e').
narrative_ontology:cs_reading_relation('8c7f2034-cc22-4151-84ee-058b5325179e', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7f2034-cc22-4151-84ee-058b5325179e', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7f2034-cc22-4151-84ee-058b5325179e', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c7f2034-cc22-4151-84ee-058b5325179e', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('8c7f2034-cc22-4151-84ee-058b5325179e', foundational, shariat_fidelity_grounding_authority).
narrative_ontology:cs_axiom_status(shariat_fidelity_grounding_authority, holdable).
narrative_ontology:cs_axiom_grounding('8c7f2034-cc22-4151-84ee-058b5325179e', shariat_fidelity_grounding_authority, theological).
narrative_ontology:cs_axiom('8c7f2034-cc22-4151-84ee-058b5325179e', foundational, communal_autonomy_preservation).
narrative_ontology:cs_axiom_status(communal_autonomy_preservation, holdable).
narrative_ontology:cs_axiom_grounding('8c7f2034-cc22-4151-84ee-058b5325179e', communal_autonomy_preservation, conventional).
narrative_ontology:cs_axiom('8c7f2034-cc22-4151-84ee-058b5325179e', secondary, gender_asymmetry_divinely_ordained).
narrative_ontology:cs_axiom_status(gender_asymmetry_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('8c7f2034-cc22-4151-84ee-058b5325179e', gender_asymmetry_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('8c7f2034-cc22-4151-84ee-058b5325179e', quranic_hadiths_as_immutable_authority).
narrative_ontology:cs_drift_state('8c7f2034-cc22-4151-84ee-058b5325179e', contemporary_2025_constitutional_review_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c7f2034-cc22-4151-84ee-058b5325179e', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, islamic_scholars_qazis).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, religious_minorities_within_muslim_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, children_without_inheritance_parity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_diaspora_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_diaspora_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parallel governance bodies that interpret and administer Shariat-based marriage, divorce, inheritance, and guardianship law. They maintain communal authority by claiming fidelity to Quranic and hadith principles, adjudicate family disputes without state interference, and resist state codification as cultural imperialism. Their institutional survival depends on maintaining jurisdictional boundaries against both civil courts and secular codification pressures.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Access unilateral talaq (oral divorce without cause or process), legal polygamy (up to four wives without wife consent), presumptive guardianship of children, and preferential inheritance shares. They also bear obligation of financial maintenance (nafaqah). Their exit options are constrained by community social cost of abandoning the framework; their structural position is asymmetrically favorable.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_household_heads, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, male_household_heads, payer).

% Subject to unilateral divorce initiation by husbands, restricted to divorce by consent or judicial grounds (higher burden), exclusion from guardianship of minor children post-divorce, lower inheritance shares (half of male equivalents), and restricted remarriage waiting periods. Exit mechanisms exist (judicial khul'a) but require male consent or proof of harm to a qazi, raising information and institutional barriers. Exit from the framework itself means community ostracism and identity rupture for many women who fuse religious identity with legal status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women, payer,
    powerless, biographical, identity_locked, national).

% Interpret Shariat texts, adjudicate disputes, legitimate the constraint through scholarly authority. Their institutional position depends on maintaining interpretive monopoly and resisting state judicial review. Some qazis collect fees for adjudication; all derive status and social influence from their role as textual authorities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, islamic_scholars_qazis, agenda_setter,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, islamic_scholars_qazis, beneficiary).

% Civil courts operate parallel authority claiming constitutional jurisdiction over family matters. They hear appeals from personal law boards on jurisdictional grounds, review enforcement of talaq and maintenance orders, and increasingly adjudicate constitutional challenges (gender equity, right to equality) against personal law provisions. Their power is structural; their exit is constrained by constitutional text and political pressure from multiple religious communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_judiciary, observer,
    institutional, generational, analytical, national).

% Hindu and secular Indian political constituencies that have historically pushed for uniform civil code, viewing personal law systems as feudal remnants or obstacles to national integration. They are structurally excluded from personal law boards' authority (the boards are communal, not representative of the polity) but have leveraged state legislative power to limit board jurisdiction and impose judicial oversight.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, political_majority, excluded,
    organized, generational, constrained, national).

% Rely on personal law boards to preserve cultural identity and community coherence in a context where they are a religious minority. They frame the constraint as protecting communal self-determination against majoritarian law. Their structural position is asymmetric: they benefit from the framework as a preservation mechanism while some members (women, children) bear extractive costs within it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_diaspora_communities, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, muslim_diaspora_communities, payer).

% Muslim scholars and activists advocating for reformed interpretations (egalitarian talaq, equal inheritance, women's guardianship) are structurally excluded from orthodox personal law board adjudication. They challenge the interpretive monopoly but lack institutional power to shift the administered law. Their exclusion is active: boards reject reformist readings as un-Islamic.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, reform_movements_within_islam, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, male_household_heads).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates family formation, dissolution, property division, and guardianship within a religious framework that preserves Islamic legal principles and communal autonomy. The arrangement centralizes marriage/divorce authority so participants know which law applies, derives legitimacy from Quranic and hadith texts, and allows communal adjudication without state apparatus.
% TRANSFER_FUNCTION: Moves family law authority (adjudicative power, interpretive control) from the state to communal institutions; moves divorce initiation power asymmetrically to male household heads; moves inheritance distribution from equal partition to gender-differentiated shares; moves guardianship authority to male relatives by default. All transfers are justified as fidelity to Quranic principles; all are contested by reform movements and secular authorities as mechanisms of gender extraction.
% ABSENT_VOICES: Reform-movement Muslim scholars, Muslim women's rights organizations, non-Muslim spouses in interfaith marriages, and children born to such unions are structurally absent from personal law board adjudication. Their exclusion is maintained by the boards' claim that reformed or secularized interpretations are un-Islamic and beyond the board's mandate. Their presence would argue for gender-equal interpretation, state oversight, and opt-in rather than communal-default authority.
% DISAPPEARANCE_RATIONALE: Personal law board authority persists through institutional inertia, political stalemate (no majority supports abolition; Hindu majority resists uniform civil code that might constrain Hindu law), and communal organization around identity preservation. If it disappeared, the state would assume exclusive family law jurisdiction — a reorganization that would affect hundreds of millions of people. Whether this reorganization would benefit or harm Muslim women, families, and communal autonomy is contested: secular authorities claim state courts offer better gender protection; communal authorities claim state law would be culturally illegitimate and impose majoritarian norms.
% FOUNDING_PROBLEM: Post-independence India faced competing claims: religious minorities demanded self-determination in personal law (constitutional protection of minority rights); secular/Hindu-majority constituencies demanded uniform national law (constitutional equality, national integration). The arrangement preserved both: minority personal laws continue with state recognition and constitutional protection, but subject to increasing judicial scrutiny on gender-equity grounds.
% FOUNDING_PROBLEM_CORROBORATION: Personal law boards attest the founding problem (communal autonomy preservation) remains live: uniform civil code threats persist and cultural assimilation pressures continue. Constitutional courts, women's rights NGOs outside the boards, and some state legislatures attest the founding problem is contested: the gender inequity and inequality dimensions have become structurally more salient than the autonomy dimension, such that 'preserving communal authority' now primarily preserves male authority within families rather than Muslim community authority within the state. Religious reformists from within the Muslim community attest the current form of personal law no longer solves the stated founding problem — it now primarily functions to entrench patriarchal interpretations against reformed readings.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at 2025) because unilateral talaq, inheritance asymmetry, and guardianship defaults concentrate family law authority and property rights in male hands; these are justified as Islamic but systematically advantage men over women. The justification is sincere (boards frame it as fidelity to Quranic principles) but the result is asymmetric extraction from women and children. Suppression is moderate-high (0.61) because the constraint persists through institutional resistance to state oversight, active exclusion of reform voices, and community social enforcement (women and men face social costs for rejecting the framework). The suppression is partly structural (legal barriers to court access, informational barriers about khul'a rights) and partly internalized (women raised in the framework often accept unequal rules as religiously legitimate). Theater has increased from 0.25 (1950) to 0.42 (2025) because boards now spend significant effort defending their authority against judicial review — adjudication is increasingly theatrical performance of compliance with constitutional law rather than pure application of Shariat. The trajectory shows: (1) initial coordination function (1950s–1970s), (2) accumulating extraction as gendered asymmetries compound (1980s–2005), (3) theater increase as state judicial pressure mounts (2005–present). The plateau in extractiveness and suppression after 2015 reflects institutional stasis: boards have hardened resistance to reform, courts have stopped major doctrinal shifts (short of legislative change), and the conflict remains structurally unresolved.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter perspective (personal law boards, Islamic scholars): the constraint is genuine coordination that preserves Islamic law and communal autonomy against majoritarian pressure. Shariat is framed as divinely ordained, not contingent or extractive. Exit from the framework is framed as apostasy or cultural surrender. From the payer perspective (Muslim women, especially those without economic independence): the constraint is enforced extraction masked as religious duty. Exit is possible (civil courts, khul'a) but carries severe information barriers, institutional friction, and community sanctions. From the observer perspective (state judiciary, constitutional scholars): the constraint is a contested kernel with two defensible readings: the communal-autonomy reading (boards are legitimate minority-rights guardians) and the rights-protection reading (all family law must satisfy constitutional equality, regardless of religion). The engine's per-seat classification should diverge: from the institutional beneficiary seat it may compute as rope (genuine coordination with modest extraction); from the powerless-payer seat it should compute as snare or tangled_rope (high extraction with structural suppression); from the observer seat it should compute as contested (the classification depends on which sibling reading you adopt). This divergence is the signal the story exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim personal law boards and Islamic scholars sit near the beneficiary end (d ≈ 0.1–0.2): they set the agenda, collect authority and status, encounter minimal exit friction (their institutional position survives because the arrangement survives). Male household heads sit near the symmetric/modest-target end (d ≈ 0.4–0.5): they benefit from asymmetric family law rules but also bear maintenance obligations and increasingly face social pressure to reform. Muslim women (especially without economic independence) sit near the full-target end (d ≈ 0.8–0.9): the constraint's rules asymmetrically govern their life choices, their exit is structurally expensive (identity_locked: fusion of religious and legal identity makes exit feel like apostasy), and their alternatives are suppressed (courts' authority is contested, qazi discretion on khul'a is arbitrary, social sanctions for rejection are severe). The state judiciary sits at the observer end (d ≈ 0.5, analytical power atom): it has structural power but ambiguous directionality — judges both enforce and increasingly question the constraint's authority. This directionality profile justifies tangled_rope classification: the constraint genuinely coordinates (preserves Quranic principles, defines family formation) but extraction is asymmetric (male-beneficiary, female-payer structure) and enforced (boards actively exclude reform voices, women face suppression in exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communal autonomy preservation) was live in 1950 — the Indian Constitution explicitly recognized minority personal laws and courts were deferential to religious communities. By 2025, the founding problem has become contested: (1) communal autonomy has been meaningfully constrained by constitutional review (courts now routinely examine personal law on gender-equity grounds, e.g., triple talaq bans, maintenance orders), and (2) the arrangement's primary effect now is preserving male authority within families, not Muslim communal authority within the state. The mandatrophy signal is: founding_problem_status = live (boards still claim the autonomy rationale) + disappearance_verdict = contested (courts and reformers disagree whether abolishing board jurisdiction would help or harm) + theater_ratio increase (0.25 → 0.42) = a constraint whose original mandate has partially atrophied but whose institutional form persists through inertia and political stalemate. A tangled_rope with atrophying coordination function and hardening extraction. Not quite a piton (the coordination function is still real and defended) but moving toward it. The constraint would resolve into a clearer type if: (1) courts abolished personal law board jurisdiction (would unmask as snare from the women-payer perspective), (2) boards reformed to gender-equal interpretation (would move toward rope), or (3) uniform civil code passed (would dissolve the kernel constraint entirely and replace it with secular law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_authenticity_vs_constructed,
    'Is the constraint a natural emergence of authentic Quranic/hadith principles, or a constructed institutional form that selects and freezes certain readings while suppressing alternatives (like egalitarian reinterpretations)?',
    'Genealogical analysis of personal law board formation, their interpretive choices over time, and comparative study of how other Muslim-majority jurisdictions interpret the same texts differently. If boards suppress legitimate alternative readings and have concentrated authority historically in conservative factions, this suggests construction rather than authenticity.',
    'If constructed, the constraint''s extraction is less justified by ''natural law'' and more visible as institutional exercise of power — shifts type from tangled_rope toward snare. If authentic in some principled sense, the extraction becomes a justified cost of fidelity to tradition rather than arbitrary power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_authenticity_vs_constructed, conceptual, 'Whether Shariat-based personal law is an authentic expression of Islamic principles or a constructed institutional form that privileges certain interpretations.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.61) primarily structural (legal barriers to court access, high khul''a burden, deficient state enforcement of women''s rights) or internalized (women accept unequal rules as religiously legitimate, have fused identity with legal status)?',
    'Longitudinal study of women who exit the personal law system (convert to another faith, migrate to secular jurisdiction, successful khul''a): do they report suppression persists after exit (internalized) or dissipates (structural)? Compare suppression metrics between first-generation immigrants (high internalization) and diaspora second-generation (more secularized).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — women carry it with them after exit, making reformation harder. If structural, removing legal barriers could substantially reduce suppression and enable exit. Affects whether the path to reform requires cultural change (internalization) or legal change (structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression in the constraint is structural (institutional barriers) or internalized (identity fusion, internalized religious legitimacy).').

omega_variable(
    communal_autonomy_vs_patriarchal_entrenchment,
    'Does personal law board authority primarily serve communal autonomy (protecting minority self-determination) or primarily preserve patriarchal entrenchment (maintaining male household authority that would erode under gender-equal state law)?',
    'Comparative institutional analysis: (1) track whether boards'' primary institutional resistance targets state gender-equity reforms specifically or state jurisdiction generally; (2) examine whether boards would accept state codification of Shariat if it preserved gender asymmetries; (3) analyze whether boards'' resistance to reform movements is equal or concentrated on feminist reinterpretations.',
    'If primarily patriarchal entrenchment, the ''communal autonomy'' framing is a cover story for extraction — classification leans toward snare. If genuinely about autonomy, the gender asymmetry is a side effect of preserving minority self-determination — classification remains tangled_rope (coordination + asymmetric extraction both real). This determines whether abolishing board authority would primarily benefit women (suggesting snare) or primarily impose majoritarian cultural dominance (suggesting legitimate minority protection).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_autonomy_vs_patriarchal_entrenchment, conceptual, 'Whether the constraint''s core function is communal autonomy preservation or patriarchal entrenchment (or both inseparably).').

omega_variable(
    kernel_reading_contest_irreducible,
    'Is the contest between this reading and its siblings (secular_civil_reading in particular) a clash of logically compatible frameworks that could coexist in one legal order (coexists_with), or does one reading logically foreclose the other within a single constitutional framework?',
    'Examine whether a genuine plural legal order with competing authorities can persist or whether the Indian constitutional structure demands ultimate adjudicative supremacy (suggesting foreclosure). If courts can defer to personal law boards while also holding constitutional review power, coexistence is stable; if courts must ultimately adjudicate all constitutional questions, one reading forecloses the other.',
    'If the readings coexist, the constraint persists as long as political stalemate holds (no majority sufficient to abolish personal law boards). If foreclosure is true, the constraint faces inevitable pressures toward one reading or the other — either boards lose authority to courts or courts recognize boards as supreme (unlikely). Affects whether the constraint is stable or inherently transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_irreducible, conceptual, 'Whether the Shariat-based reading of marriage authority logically coexists with secular civil readings or one forecloses the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(marr_tr_t1975, observed).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement_basis(marr_tr_t1990, observed).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(marr_tr_t2005, observed).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(marr_tr_t2015, observed).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(marr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1975, 0.61).
narrative_ontology:measurement_basis(marr_be_t1975, observed).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 1990, 0.64).
narrative_ontology:measurement_basis(marr_be_t1990, observed).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement_basis(marr_be_t2005, observed).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(marr_be_t2015, observed).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(marr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement_basis(marr_su_t1975, observed).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 1990, 0.57).
narrative_ontology:measurement_basis(marr_su_t1990, observed).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(marr_su_t2005, observed).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement_basis(marr_su_t2015, observed).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement_basis(marr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested marriage authority kernel in post-independence India. The kernel is the constitutional arrangement recognizing minority personal law authority. This reading anchors authority in Shariat interpretation by communal boards and qazis. Sibling readings decompose the same kernel into: hindu_codified_reading (state-codified Hindu law), christian_canonical_reading (canonical church law), parsi_communal_reading (communal custom), secular_civil_reading (constitutional civil code). Each reading produces a different constraint story with different ε values, beneficiary/victim structures, and types. The ε-invariance principle applies: these are NOT one constraint viewed from different angles, but multiple constraints sharing a common institutional kernel. Each story is ε-invariant internally; differences across readings reflect structural differences in authority grounding and extraction mechanisms, not measurement ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
