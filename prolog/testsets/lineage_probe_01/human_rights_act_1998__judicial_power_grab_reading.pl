% ============================================================================
% CONSTRAINT STORY: human_rights_act_1998__judicial_power_grab_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_rights_act_1998__judicial_power_grab_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_rights_act_1998__judicial_power_grab_reading
 *   human_readable: HRA Section 3 Judicial Reinterpretation: Power Transfer to Courts
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The Human Rights Act 1998 is presented in conventional UK legal discourse
 *   as having three structurally distinct meanings, each claimed by different
 *   institutional actors. This constraint instantiates ONE reading: the
 *   judicial_power_grab reading, which asserts that section 3 HRA has
 *   transferred effective legislative power to courts by requiring them to
 *   interpret statutes compatibly with Convention rights, thereby rewriting
 *   enacted meanings without parliamentary consent. The kernel — the HRA text
 *   itself — is fixed and formally immutable, but its institutional meaning
 *   is contested across three live readings: incorporation (the HRA
 *   domesticated rights, enabling courts to enforce them; the point is rights
 *   protection), parliamentary sovereignty preserved (courts may declare
 *   incompatibility but cannot strike down; Parliament's last word survives),
 *   and judicial power grab (courts reinterpret statutes in ways Parliament
 *   dare not ignore; declarations of incompatibility are strike-downs in
 *   practice). This constraint analyzes the power_grab reading as a
 *   tangled_rope: it carries both genuine coordination function (harmonizing
 *   statutes with rights values) AND asymmetric extraction (relocating
 *   political choice from legislature to court). The extractiveness has risen
 *   from 0.35 at HRA enactment to 0.58 presently, as courts have developed
 *   increasingly expansive section 3 jurisprudence (Ghaidan, Chester, Re G
 *   (Adoption: Unmarried Couple)) that rewrites statutory meaning far from
 *   the text's ordinary language interpretation. Suppression has risen (0.48
 *   → 0.62) as Parliament faces mounting costs to override section 3
 *   interpretations: the override requires explicit statutory language,
 *   generates immediate legal challenge, and creates political costs of
 *   appearing to reject rights values. Theater has declined (0.55 → 0.38)
 *   because the mechanism has matured from performative parliamentary debate
 *   about rights to functional relocation of policy choice from legislature
 *   to bench.
 *
 * KEY AGENTS:
 *   - Senior Judiciary: Beneficiary (institutional/arbitrage) — gains effective power to shape statutory meaning through section 3 interpretive obligation, framed as judicial duty
 *   - Enacted Legislative Intent: Victim (powerless/trapped) — the meaning Parliament chose to enact is reinterpreted retroactively without mechanism for legislative override that is not suppressed
 *   - Parliament (Legislative Majorities, General): Victim (moderate/constrained) — subsequent legislatures inherit section 3 constraint; override is formally available but politically and legally expensive
 *   - Rights-Advocating Community: Mixed (organized/constrained) — benefit from domestic rights enforcement pathway, but dependent on judiciary's interpretive choices
 *   - Executive Government: Mixed (institutional/constrained) — experiences extraction of discretion alongside coordination benefits when rights-respecting
 *   - Analytical Observer: (analytical/analytical) — risks naturalizing a contingent institutional arrangement (judicial review) as structural feature of constitutionalism itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_rights_act_1998__judicial_power_grab_reading, 0.58).
domain_priors:suppression_score(human_rights_act_1998__judicial_power_grab_reading, 0.62).
domain_priors:theater_ratio(human_rights_act_1998__judicial_power_grab_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_rights_act_1998__judicial_power_grab_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(human_rights_act_1998__judicial_power_grab_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(human_rights_act_1998__judicial_power_grab_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_rights_act_1998__judicial_power_grab_reading, tangled_rope).
narrative_ontology:human_readable(human_rights_act_1998__judicial_power_grab_reading, "HRA Section 3 Judicial Reinterpretation: Power Transfer to Courts").
narrative_ontology:topic_domain(human_rights_act_1998__judicial_power_grab_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(human_rights_act_1998__judicial_power_grab_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_rights_act_1998__judicial_power_grab_reading, 'f0c819fa-d500-41b2-81f9-578b63ab0af4').
narrative_ontology:cs_kernel_codification('f0c819fa-d500-41b2-81f9-578b63ab0af4', formalized).
narrative_ontology:cs_authority_grounding('f0c819fa-d500-41b2-81f9-578b63ab0af4', extraction).
narrative_ontology:cs_interpretation_layer_present('f0c819fa-d500-41b2-81f9-578b63ab0af4').
narrative_ontology:cs_reading_relation('f0c819fa-d500-41b2-81f9-578b63ab0af4', human_rights_act_1998__incorporation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0c819fa-d500-41b2-81f9-578b63ab0af4', human_rights_act_1998__parliamentary_sovereignty_preserved_reading, coexists_with).
narrative_ontology:cs_axiom('f0c819fa-d500-41b2-81f9-578b63ab0af4', foundational, courts_rewrite_statutory_meaning_through_section_3).
narrative_ontology:cs_axiom_status(courts_rewrite_statutory_meaning_through_section_3, holdable).
narrative_ontology:cs_axiom_grounding('f0c819fa-d500-41b2-81f9-578b63ab0af4', courts_rewrite_statutory_meaning_through_section_3, empirically_contingent).
narrative_ontology:cs_axiom('f0c819fa-d500-41b2-81f9-578b63ab0af4', foundational, parliament_override_capacity_is_structurally_suppressed).
narrative_ontology:cs_axiom_status(parliament_override_capacity_is_structurally_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('f0c819fa-d500-41b2-81f9-578b63ab0af4', parliament_override_capacity_is_structurally_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('f0c819fa-d500-41b2-81f9-578b63ab0af4', parliamentary_legislative_monopoly).
narrative_ontology:cs_drift_state('f0c819fa-d500-41b2-81f9-578b63ab0af4', contemporary_expanded_section_3_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f0c819fa-d500-41b2-81f9-578b63ab0af4', '').
narrative_ontology:cs_kernel_id(human_rights_act_1998__judicial_power_grab_reading, human_rights_act_1998).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_rights_act_1998__judicial_power_grab_reading, senior_judiciary).
narrative_ontology:constraint_victim(human_rights_act_1998__judicial_power_grab_reading, enacted_legislative_intent).
narrative_ontology:constraint_victim(human_rights_act_1998__judicial_power_grab_reading, parliament_effective_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENACTED LEGISLATIVE INTENT (SNARE) — The meaning Parliament enacted into statute is trapped. Section 3 obligation requires courts to read and give effect to legislation in a way compatible with Convention rights — this reinterprets statutory meaning retroactively without legislative consent or mechanism. Parliament cannot escape this reinterpretation; the original enactment's textual force is suppressed by judicial obligation. No exit option; full extraction of political choice authority. Powerless victim bearing maximum suppression.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LATER-ENACTED PARLIAMENT (SNARE) — Subsequent legislatures find themselves constrained by the interpretive obligation they inherit. When they enact new legislation, section 3 courts read rights-compatibility into the statute's meaning regardless of what the enacting Parliament chose to specify. Parliament can theoretically re-legislate to override an incompatible court interpretation, but this requires explicit statement (per Ghaidan v Godin-Mendoza principle) and generates immediate judicial skepticism and rights-based challenges. The later Parliament's exit cost is very high: budgetary, political, and litigation exposure. Constrained but not trapped — the pathway exists but is heavily suppressed.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SENIOR JUDICIARY (ROPE) — From the court's perspective, section 3 is a coordination mechanism: it coordinates judicial interpretation toward rights-compatibility without eliminating Parliament's legislative capacity entirely. Courts frame the power as interpretive (a duty to read compatibly) rather than legislative (striking down statutes). This framing creates arbitrage — the judiciary gains effective power to rewrite statutes' meaning while preserving the parliamentary sovereignty narrative. The mechanism appears as genuine coordination (harmonizing statutes with rights) rather than extraction, though the structural effect is to relocate political choice from legislature to court. Institutional power with arbitrage exit: courts can interpret or avoid interpretation, apply section 3 strongly or narrowly, depending on strategic calculus.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHTS-ADVOCATING COMMUNITY (TANGLED ROPE) — Organizations and individuals pursuing rights claims benefit from the HRA's domestic enforcement pathway: no need for the long road to Strasbourg, faster remedy, cheaper access. Section 3 courts are more willing to grant relief than pre-HRA courts were. BUT they are also constrained by the courts' own strategic choices about when to invoke section 3 strongly vs narrowly. They cannot force judicial creativity; they depend on sympathetic judges and favorable precedent. Organized (have collective voice), constrained (depend on judiciary's discretion), with genuine coordination benefits (the HRA created a new pathway) AND asymmetric extraction (they do not control the courts' interpretive choices). Genuine coordination + extraction hybrid at generational timescale — the community has agency but less than they would have if they controlled the courts directly.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXECUTIVE GOVERNMENT (TANGLED ROPE) — The executive experiences section 3 as both coordinating and extracting. Courts can reinterpret legislation the executive drafted, imposing compliance costs. But the executive also benefits from the judicial reinterpretation when it aligns with the executive's rights-respecting agenda (and executive governments often do pursue rights-based policy). The executive is constrained — it cannot easily override courts' section 3 interpretations without re-legislation, and legislative override generates political costs. Genuine coordination function (rights compliance) mixed with extraction function (loss of executive discretion over statutory meaning). Institutional power but constrained exit (must legislate to override, which is expensive). Tangled rope captures this mixed experience.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, judicial review of legislation for rights-compatibility is an immutable structural feature of modern constitutionalism: separation of powers creates institutional tension, rights require judicial enforcement, and that enforcement necessarily involves courts saying what the law is. This perspective treats the power transfer as not a transfer at all but a natural institutional feature of constitutionalism itself. However, the structural data contradicts this: the HRA is a legislative choice, the section 3 mechanism is specific to the 1998 Act, and different constitutional designs achieve rights protection without relocating political choice to courts (e.g., parliamentary rights bills, advisory reference systems). The mountain classification is perspectival — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_rights_act_1998__judicial_power_grab_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_rights_act_1998__judicial_power_grab_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_rights_act_1998__judicial_power_grab_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_rights_act_1998__judicial_power_grab_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The core extractive mechanism is the relocation of policy choice from legislature to court. When courts apply section 3 to reinterpret statutory meaning, they are making political choices (what does this statute mean in rights context?) that Parliament delegated to itself in the original enactment. The 0.58 value reflects that this is genuine extraction (Parliament loses control of enacted meaning) but not maximal (Parliament retains formal override capacity, courts sometimes show restraint, and the coordination function is real — harmonizing statutes with rights is a legitimate goal). The rise from 0.35 to 0.58 over 14 years reflects judicial confidence in the power: early courts applied section 3 conservatively; recent courts apply it expansively (Ghaidan required reinterpretation that the statute's ordinary language could not support). Suppression (0.62): High. Parliament faces multiple barriers to override: political barriers (appearing to reject rights values), legal barriers (courts scrutinize explicit override language intensely), and structural barriers (Parliament did not anticipate needing to legislate to preserve its own meaning; override is reactive, not proactive). The rise from 0.48 to 0.62 reflects increasing rigidity of the suppression: courts have now established that section 3 applies to all legislation including pre-HRA statutes (not just new legislation), and they have signaled that explicit override language still requires courts to interpret it narrowly (Ghaidan principle). Theater (0.38, declining): The mechanism has moved from performative (parliamentary rhetoric about rights, judicial deference, appearances of restraint) to functional (courts now openly reshape statutory meaning, Parliament now openly legislates to override). The decline reflects maturation of the institutional arrangement from theater to naked power exercise. Theater was high at enactment because courts and Parliament both maintained the fiction that section 3 was merely coordinating rights-compatible interpretation; theater is lower now because everyone knows courts are rewriting statutes and Parliament is scrambling to legislate override.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies perspectival divergence arising from structural position. The enacted legislative intent sees pure extraction (Snare) because the statute's meaning is rewritten without consent. Parliament sees constrained extraction (Snare transitioning to Tangled Rope as override capacity proves more theoretical than practical). The judiciary sees coordination (Rope) — a duty to read statutes in rights-compatible ways, which is a legitimate governance goal. Rights advocates see mixed coordination and constraint (Tangled Rope) — the HRA opened a new pathway but dependence on judicial discretion limits their agency. The executive sees Tangled Rope because some reinterpretations hurt and some help, depending on alignment. The analytical observer risks seeing Mountain (immutable separation-of-powers requirement) but structural data reveals this as false summit (the HRA's design is contingent; different democracies achieve rights protection without judicial statutory reinterpretation). The gap exists because the extractiveness is real from the legislature's perspective but invisible or legitimate from the judiciary's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural relationship to extraction. Enacted legislative intent is the victim: its d is high (~0.95), producing maximum experienced extraction. Parliament is a mixed victim-constrained actor: its d is moderate-high (~0.65), experiencing substantial but not maximal extraction (override exists in principle). The judiciary is the beneficiary with arbitrage: its d is low (~0.15), experiencing negative chi (extraction flows toward them). Rights advocates are organized beneficiaries but constrained by dependence on judicial discretion: their d is moderate-low (~0.35), experiencing some extraction cost (dependence on court discretion) but offset by coordination benefits (domestic pathway). The engine derives d from these structural relationships and applies the sigmoid f(d) to compute experienced extractiveness chi. The analytical observer at organizational power derives d from the canonical table (~0.72), producing high chi at the civilizational scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint would collapse into mandatrophy if it attempted to answer 'is section 3 coordination or extraction?' as a single-perspective question. The judicial_power_grab reading resolves the mandatrophy by asserting that section 3 IS extraction (from the legislative intent and Parliament's perspective) AND coordination (from the judiciary's and rights advocates' perspectives), simultaneously. The tangled_rope classification captures this hybrid: the constraint exhibits both genuine coordination function (harmonizing statutes with rights) and asymmetric extraction (relocation of political choice to courts). The mandatrophy is not 'which perspective is correct?' but 'what is the structural relationship, seen from each perspective?' Tangled rope is the answer because all the structural data — beneficiary/victim structure (judiciary benefits, Parliament loses), enforcement mechanism (courts actively reinterpret, Parliament must react), and measurement trajectory (extraction rising, theater declining) — are consistent with a hybrid in the tangled_rope range, not pure extraction and not pure coordination. The judicial_power_grab_reading preserves this hybrid: it does not claim section 3 is pure snare (that would be the pure_extraction_reading); it claims section 3 is tangled_rope with a power-transfer vector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_3_interpretive_boundary,
    'Where does legitimate statutory interpretation end and legislative rewriting begin under section 3 HRA?',
    'Analysis of case law establishing the outer bounds of section 3 interpretation: at what textual distance from the statutory language do courts retreat? Does the boundary hold across issue domains and bench composition changes? Does the boundary correspond to judicial institutional interests or to principled interpretive doctrine?',
    'If boundary is principled and stable: section 3 is coordination with defined limits (Rope from some perspectives). If boundary is strategic and shifts with bench composition: section 3 is institutional power-seeking (Snare tendency from legislative intent perspective). If boundary is doctrine-dependent but unprincipled: extraction mechanism is obscured (Tangled Rope confirmed, but with higher suppression component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_3_interpretive_boundary, empirical, 'Interpretive boundary between legitimate reading and legislative rewriting').

omega_variable(
    parliament_override_feasibility,
    'Can Parliament effectively override section 3 court interpretations, or is the override pathway sufficiently suppressed to make it a dead letter?',
    'Empirical analysis of Parliamentary override attempts post-HRA: How many section 3 interpretations have been followed by legislative override? What is the track record of override success vs failure? What political and institutional barriers prevent override? Compare frequency of override to constitutional systems with clearer override mechanisms (e.g., notwithstanding clauses).',
    'If override is feasible and used: Snare classification weakens; Parliament retains effective choice (Rope/Tangled Rope confirmed). If override is suppressed (politically costly, legislatively difficult, rare in practice): Snare classification for Parliament is confirmed; extraction is structural, not illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliament_override_feasibility, empirical, 'Feasibility of Parliamentary override of section 3 interpretations').

omega_variable(
    reading_contest_resolution,
    'Which reading of the HRA kernel — incorporation, judicial power grab, or parliamentary sovereignty preserved — represents the actual institutional dynamics?',
    'This omega documents that the HRA is a contested kernel, and this constraint instantiates ONE reading (judicial_power_grab_reading). The resolution mechanism is the network of cs_structure.reading_relations: this reading coexists_with the incorporation_reading (both are live, held by different institutional actors — Courts hold the judicial power grab reading; human rights advocates hold the incorporation reading; both can be true simultaneously). This reading forecloses the parliamentary_sovereignty_preserved_reading at the structural level: if section 3 actually relocates political choice to courts (this reading), then Parliament''s ''last word'' is illusory (contradicts the sibling''s core premise). However, the parliamentary_sovereignty_preserved_reading remains live as a normative claim about what Parliament *should* retain, even if institutionally it has not. The contest is real and unresolved.',
    'If incorporation_reading is dominant institutionally: courts see rights enforcement as the HRA''s point, not power-seeking; extraction framing weakens. If judicial_power_grab_reading is dominant: judges frame their power as interpretive duty, creating suppression of legislative meaning. If parliamentary_sovereignty_preserved_reading becomes operational: Parliament successfully asserts last word, converting section 3 from extraction mechanism to coordination tool. All three remain live; the reading you are generating instantiates one specific framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_resolution, conceptual, 'Kernel contest: which reading of HRA reflects institutional reality').

omega_variable(
    judicial_institutional_interests_entanglement,
    'To what extent does the judiciary''s institutional expansion through section 3 depend on genuine rights-protection values vs institutional power-seeking?',
    'Behavioral analysis: Do courts apply section 3 systematically (apply it equally across issue domains, bench composition, claimant backgrounds)? Or do they deploy it strategically (more aggressive in rights-friendly cases, less aggressive in deferential areas, varying with senior judge preferences)? Does the pattern of section 3 application align with rights-protection logic or institutional expansion logic?',
    'If systematic and values-driven: the rope coordination interpretation (judges see duty, not power) is confirmed, reducing the Snare classification to constrained-Tangled-Rope. If strategic and power-seeking: the judicial_power_grab_reading is confirmed; extraction framing is justified; Snare classification for Parliament is hardened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_institutional_interests_entanglement, empirical, 'Whether judicial section 3 behavior reflects rights values or institutional power-seeking').

omega_variable(
    cross_reading_axiom_conflict,
    'Does the judicial_power_grab_reading''s core axiom (courts have effective legislative power through section 3) foreclose or coexist with the parliamentary_sovereignty_preserved_reading''s core axiom (Parliament retains final legal authority)?',
    'Jurisprudential analysis: Can both axioms be held in a single coherent legal framework? The answer is: yes, they can, if courts frame section 3 as interpretive (not legislative) and Parliament retains override capacity (formal sovereignty). But they create permanent tension. If courts regularly apply section 3 in ways that require explicit statutory override to reverse, and Parliament rarely or never exercises override, the axioms drift toward incompatibility in practice. The empirical resolution comes from oracle_parliament_override_feasibility and section_3_interpretive_boundary omegas.',
    'If axes coexist coherently: both readings remain live, with different institutional actors holding each. If axes become practically incompatible: one reading forecloses the other institutionally, though not logically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_reading_axiom_conflict, conceptual, 'Axiom conflict between power-grab and sovereignty-preserved readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_rights_act_1998__judicial_power_grab_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hra_pgrab_theater_t0, human_rights_act_1998__judicial_power_grab_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hra_pgrab_theater_t7, human_rights_act_1998__judicial_power_grab_reading, theater_ratio, 7, 0.42).
narrative_ontology:measurement(hra_pgrab_theater_t14, human_rights_act_1998__judicial_power_grab_reading, theater_ratio, 14, 0.38).

% Extraction over time
narrative_ontology:measurement(hra_pgrab_extract_t0, human_rights_act_1998__judicial_power_grab_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hra_pgrab_extract_t7, human_rights_act_1998__judicial_power_grab_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(hra_pgrab_extract_t14, human_rights_act_1998__judicial_power_grab_reading, base_extractiveness, 14, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hra_pgrab_suppress_t0, human_rights_act_1998__judicial_power_grab_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(hra_pgrab_suppress_t7, human_rights_act_1998__judicial_power_grab_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(hra_pgrab_suppress_t14, human_rights_act_1998__judicial_power_grab_reading, suppression_requirement, 14, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_rights_act_1998__judicial_power_grab_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_rights_act_1998__judicial_power_grab_reading, human_rights_act_1998__incorporation_reading).
narrative_ontology:affects_constraint(human_rights_act_1998__judicial_power_grab_reading, human_rights_act_1998__parliamentary_sovereignty_preserved_reading).

% DUAL FORMULATION NOTE:
% The HRA kernel is contested across three structurally distinct readings, each generating a separate constraint story with different ε values and classification profiles. The judicial_power_grab_reading (this file) analyzes the HRA as tangled_rope with extractiveness 0.58. The incorporation_reading would analyze the HRA as rope with lower extractiveness (rights protection as coordination). The parliamentary_sovereignty_preserved_reading would analyze the HRA as scaffold or rope with sunset clause, emphasizing Parliament's formal override mechanism. All three are linked via network.affects_constraints to document constraint family membership. Each story's cs_structure includes reading_relations establishing how the three readings relate structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
