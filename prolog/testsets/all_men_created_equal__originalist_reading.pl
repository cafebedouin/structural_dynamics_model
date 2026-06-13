% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality Bounded by Founders' Intent (Originalist Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Declaration of Independence and the Constitution use universal
 *   equality language ('all men are created equal'; 'equal protection of the
 *   laws') but the founding generation applied these principles narrowly,
 *   excluding enslaved people, women, indigenous peoples, and
 *   non-property-holding men. The originalist reading of these texts (ONE
 *   reading of a contested kernel) resolves this gap by canonizing the
 *   founders' 18th-century social taxonomy as the binding meaning: equality
 *   means what the founders meant, which is bounded by their categories. This
 *   reading benefits founding-class descendants and constitutional
 *   conservatives by locking in a narrow definition of equality that requires
 *   amendment to expand. It extracts from historically excluded groups by
 *   denying them the literal scope of the universal language. The originalist
 *   reading claims to be principle-driven (fidelity to original meaning) but
 *   is functionally extractive: its beneficiaries are the founding elite and
 *   their legal/political heirs; its victims are those excluded by the
 *   founders' taxonomy and their descendants. The constraint persists because
 *   it provides interpretive authority and stability to a conservative
 *   coalition; it requires active enforcement (suppressing alternative
 *   readings and the universalist reading's claim that equality language
 *   should expand).
 *
 * KEY AGENTS:
 *   - founding_male_property_holders: Primary beneficiaries (1770s-1780s); authored the constraint by declaring universal language they applied narrowly; secured legal supremacy in perpetuity through originalist interpretation.
 *   - enslaved_africans_and_descendants: Primary victims (structural powerlessness, identity-locked to 18th-century taxonomy); excluded from equality claim by the constraint's enforcement; bear extraction through centuries of systemic subordination.
 *   - women: Secondary victims (moderate power, constrained exit via amendment); excluded from founding's equality claim; must overcome originalist frame to expand rights.
 *   - judicial_originalists: Agenda-setters (institutional power); enforce the reading through doctrine; benefit from authority and stability the constraint provides.
 *   - constitutional_conservative_coalition: Beneficiaries and co-agenda-setters (organized power); champion originalism because it locks in narrow equality definitions; have mobile exit but choose to maintain the frame.
 *   - universalist_interpreters: Excluded from authority (analytical power); would dissolve the constraint by reading equality as universal and progressive; their exclusion is the enforcement mechanism.
 *   - textualist_paradox_critics: Observers (analytical); identify the logical gap between universal text and restricted originalist reading; their observation must be suppressed for the constraint to hold.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.81).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.77).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality Bounded by Founders' Intent (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '90853bb9-09cd-4406-abd7-25765c65f1ca').
narrative_ontology:cs_kernel_codification('90853bb9-09cd-4406-abd7-25765c65f1ca', fixed_text).
narrative_ontology:cs_authority_grounding('90853bb9-09cd-4406-abd7-25765c65f1ca', extraction).
narrative_ontology:cs_interpretation_layer_present('90853bb9-09cd-4406-abd7-25765c65f1ca').
narrative_ontology:cs_reading_relation('90853bb9-09cd-4406-abd7-25765c65f1ca', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('90853bb9-09cd-4406-abd7-25765c65f1ca', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('90853bb9-09cd-4406-abd7-25765c65f1ca', foundational, founders_intent_is_binding_constraint).
narrative_ontology:cs_axiom_status(founders_intent_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('90853bb9-09cd-4406-abd7-25765c65f1ca', founders_intent_is_binding_constraint, conventional).
narrative_ontology:cs_axiom('90853bb9-09cd-4406-abd7-25765c65f1ca', foundational, eighteenth_century_social_taxonomy_is_dispositive).
narrative_ontology:cs_axiom_status(eighteenth_century_social_taxonomy_is_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('90853bb9-09cd-4406-abd7-25765c65f1ca', eighteenth_century_social_taxonomy_is_dispositive, empirically_contingent).
narrative_ontology:cs_reference_frame('90853bb9-09cd-4406-abd7-25765c65f1ca', founding_generation_hierarchical_equality).
narrative_ontology:cs_drift_state('90853bb9-09cd-4406-abd7-25765c65f1ca', contemporary_post_civil_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('90853bb9-09cd-4406-abd7-25765c65f1ca', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_male_property_holders).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, judicial_originalists).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, constitutional_conservative_coalition).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, propertyless_white_men).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, founders_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, historical_meanings_binding).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, eighteenth_century_social_categories_dispositive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% White male property owners in the 1770s-1780s who authored and ratified the Declaration and Constitution. They declared equality in universalist language but applied it only to themselves and their class, explicitly excluding enslaved people, women, indigenous peoples, and non-property-holding men. The originalist reading canonizes their narrow 18th-century understanding as the binding constraint on what equality means forever. They set the interpretive frame and their class benefits from its restriction — their descendants inherit a constitutional reading that treats the founders' exclusions as permanently binding. They have high exit options (could change the interpretation) but do not exercise them because the current interpretation serves their interests.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_male_property_holders, beneficiary,
    powerful, civilizational, arbitrage, national).

% Excluded from the founding generation's equality claim by the originalist reading, which honors 18th-century taxonomy treating them as property or subhuman. The constraint's enforcement prevents the equality text from being read as applying to them. They bear the cost of exclusion through centuries of legal subordination: slavery (1776–1865), segregation (1865–1965), and ongoing systemic discrimination. The originalist frame traps them because it refuses to read the universal language universally — it insists that what 'equality' means is what the founders meant, and the founders meant it to exclude them. Exit is impossible: they cannot leave the jurisdiction; they cannot change the founders' intent; they cannot rewrite the Constitution without amending it, which the originalist frame says requires supermajority consensus.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_africans_and_descendants, payer,
    powerless, civilizational, trapped, national).

% Excluded from the founding's equality claim by originalist interpretation of 18th-century social taxonomy, which treated women as legally dependent on male heads of household. The constraint's enforcement requires reading equality as the founders understood it — which did not include women's independent legal personhood, economic agency, or political representation. They bore the cost of this exclusion through restrictions on property ownership, voting, contract-making, and custodial rights. They can exit through constitutional amendment (19th Amendment, 14th Amendment expansion), but the originalist reading makes that the ONLY legitimate path to expanded equality, which is costly and uncertain — they must build massive political coalitions and change the fundamental law rather than having courts reinterpret the existing text's universal language as applying to them.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    moderate, generational, constrained, national).

% Not contemplated in the founders' equality claim; the originalist reading honors the founders' taxonomy that treated them as outside the political community altogether — as nations, not individuals, and not persons deserving equal protection. The constraint enforces a reading of equality that canonizes their exclusion by reference to 18th-century categories. They are bound by a constitutional text whose original meaning, as originalists read it, did not include them and was not written to include them. They can organize tribal sovereignty and political resistance, but they are structurally constrained by a constitutional frame that treats the founders' exclusion of them as the dispositive meaning of equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    organized, civilizational, constrained, regional).

% Not included in the founding's narrow equality claim, which applied only to male property holders. The originalist reading treats the founders' exclusion of propertyless men as part of what 'equality' meant then — a partial inclusion in the human category but not the full political one. They gained the vote through democratic expansion (post-1828) rather than constitutional reinterpretation, which means they had to fight for rights the originalist frame never granted them. The constraint constrains what further equality claims they can make by binding meaning to the founders' hierarchical categories.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, propertyless_white_men, payer,
    moderate, biographical, constrained, national).

% Judges and legal scholars who enforce the originalist reading as the correct interpretive method. They set the rules for what the equality text can mean by insisting it means what the founders meant when they wrote it. They author doctrine (precedents, opinions, legal articles) that canonizes the originalist method. They benefit from the authority this interpretive monopoly confers — they are the gatekeepers of constitutional meaning. They benefit from the stability it provides to existing class structures. They administer the constraint through judicial review, precedent-setting, and legal scholarship that teaches lawyers and judges how to read the Constitution.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, judicial_originalists, agenda_setter,
    institutional, generational, analytical, national).

% Political and intellectual coalition (conservative legal foundations, Federalist Society, originalist scholars, conservative political movements, Republican parties) that champions originalism as the correct constitutional method. They benefit substantially from the constraint's enforcement because it locks in narrow definitions of rights, privacy, and equality, preventing expansion to marginalized groups without formal amendment. They have high exit options — they could shift to living constitutionalism, textualism, or other methods — but they choose to maintain the originalist frame because it serves their distributional and ideological goals. They understand that originalism's restriction of equality to founder-intent preserves conservative advantages in contested areas (affirmative action, voting rights, LGBTQ+ rights, immigrant status). They have mobile power (can choose their strategy) but are committed to this particular strategy because it works.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_conservative_coalition, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, constitutional_conservative_coalition, agenda_setter).

% Constitutional scholars, judges, and activists who read the equality language as a universal principle requiring iterative expansion regardless of founder intent. They are excluded from the originalist constraint's authority structure — their interpretations are treated by originalists as illegitimate deviation from proper method, not as competing readings of the same text. They would argue for reading equality as binding the nation to its universal language, which would require courts to expand rights without amendment. This would dissolve the originalist constraint entirely. Their exclusion from authority is the constraint's enforcement mechanism: the originalist coalition ensures that universalist arguments are not permitted in respectable legal discourse, that universalists do not staff the courts or lead legal institutions, and that universalism is reframed as 'activism' rather than interpretation. They have mobile exit (can teach law, write, speak, organize) but cannot exercise institutional power within the originalist-dominated judiciary.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_interpreters, excluded,
    organized, generational, mobile, national).

% Legal theorists and scholars who point out the internal logical contradiction in the originalist reading: the text says 'all men are created equal' (universal language) but originalism reads it as bound by the founders' restricted application (18th-century social taxonomy). They observe that the constraint's coherence depends on suppressing this paradox — either the language is truly universal or it was always restrictive, but the two cannot be held simultaneously without extra-textual reasoning that contradicts originalism's own stated method. They have no institutional power (analytical seat only) but have epistemic standing to identify the constraint's logical gap. They are not excluded (they can speak) but their observation is treated as a gotcha rather than a serious challenge to the method. Their role is to identify the paradox that originalism must suppress to maintain authority.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, textualist_paradox_critics, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_male_property_holders).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared interpretive frame for the Constitution that enables predictability in constitutional law: judges and litigants know what 'equality' is supposed to mean by reference to what the founders meant when they wrote it, preventing ad-hoc reinterpretation and ensuring legal stability. The constraint coordinates the judicial elite and the conservative coalition around a fixed constitutional meaning that protects existing property distributions and hierarchies.
% TRANSFER_FUNCTION: Moves interpretive authority from living majorities and from the text's universal language to the historical snapshot of meaning the founders intended. Transfers the power to define equality from contemporary democratic processes to 18th-century social categories. Extracts from historically excluded groups (enslaved people, women, indigenous peoples, propertyless men) by denying them the equality language's literal scope and requiring them to amend the Constitution to gain rights. Transfers that extraction into protection for founding-class descendants and conservative coalition members, who inherit a constitutional reading that treats the founders' exclusions as permanently binding and require no action to maintain their advantages.
% ABSENT_VOICES: Enslaved people (no representation in founding); women (not contemplated as rights-holders); indigenous peoples (not included in the political community); propertyless white men (disenfranchised and excluded from property-based equality); future generations whose equality claims would be foreclosed by binding meaning to 18th-century intent. These groups would object that equality language should be read universally, that new majorities should be able to reinterpret the text, and that the constraint denies them equal standing. But they are structurally excluded from the originalist interpretive process — their objections are treated as illegitimate appeals to non-originalist methods and are not given standing in authoritative constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished — if courts stopped treating 18th-century founder intent as the binding meaning of equality — the constitutional meaning of equality would expand immediately to include groups the founders excluded. Expanded definitions of who deserves equal protection would follow; legislative remedies for discrimination would accelerate; federal protection for voting rights, reproductive rights, LGBTQ+ rights, and immigrant rights would be much more likely; property hierarchies defended by narrow equality readings would face constitutional challenge. The political coalition benefiting from the constraint (conservative legal and political movements) would reorganize to defend its interests through other interpretive means or through democratic supermajority amendment. The entire structure of precedent built on narrow equality definitions would require re-examination. Federal courts would face a massive docket of equal protection and equal citizenship claims that the originalist constraint had foreclosed.
% FOUNDING_PROBLEM: The Declaration of Independence and the Constitution used universal equality language but the founding generation applied it narrowly. Later interpreters faced a profound choice: read the language universally (as it literally says) or read it as bound by what the founders meant when they wrote it (which was a narrow, hierarchical application). Originalism chose the latter, claiming it stabilizes constitutional meaning against arbitrary reinterpretation and prevents judges from imposing their own values.
% FOUNDING_PROBLEM_CORROBORATION: Originalists attest the problem is still live: without the constraint of founder intent, constitutional meaning would become unstable and subject to political pressure; judges would rewrite the Constitution to match their policy preferences. Constitutional conservatives defend the originalist reading as necessary to the rule of law. Universalists and critics attest the founding problem is mis-stated: the real problem is not that the text is ambiguous but that it was written with universal language the founders never intended to apply universally — originalism covers that dishonesty with a legitimacy story about fidelity. Legal historians (Foner, McPherson, Bailyn, others writing from outside the originalist coalition) document the founding generation's explicit exclusions: slavery was written into the Constitution; women's legal subordination was assumed; property requirements were imposed. Judicial dissenters document how originalism selectively applies historical fidelity — using it to restrict rights but ignoring it when the founders' actual practices (accepting slavery, limiting voting) are inconvenient. Critical race theorists and feminist legal scholars note that originalism's hidden beneficiary structure maps onto a racial and gendered hierarchy the founders installed. No party outside the originalist and conservative coalition defends the founding problem as stated by originalists — the consensus among historians, critical scholars, and universalist lawyers is that the founding problem is real but that originalism's solution serves elite interests, not interpretive principle.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint's primary function is to protect founding-class advantages and prevent downward redistribution of rights to excluded groups. The beneficiaries (founding elite, their heirs, conservative coalition) are concentrated; the victims (enslaved people, women, indigenous peoples) are numerous and powerless. Suppression is high (0.77) because the constraint requires actively keeping alternative readings (universalist, textualist-paradox) out of judicial consideration — judges must be trained to read the universal language as bound by founder intent, not as self-executing. Theater is moderate (0.42) because the constraint operates partly through genuine legal argument (originalist method has real argumentative structure) and partly through suppression of counter-arguments (the paradox critics are silenced, universalist readings are treated as illegitimate). Accessibility collapse is moderate-high (0.68): once a person understands that originalism binds meaning to 18th-century taxonomy, the impossibility of expanding rights without amendment becomes clear — alternatives collapse. Resistance is substantial (0.62) because the constraint faces ongoing challenge from universalist factions, legal scholars who identify the paradox, and social movements demanding expanded equality. The measurement series tracks extraction accumulation and theater increase over the 46-year interval (approximately 1978–2024, the era of originalism's institutional rise): as originalism became more fully institutionalized in the judiciary, extractiveness increased (more equality claims were foreclosed), theater increased (more energy spent defending the method's coherence), and suppression requirement increased (more alternative readings had to be actively excluded).
 *
 * PERSPECTIVAL GAP:
 *   From the founding elite's seat (and their conservative heirs), the constraint is genuinely principled: they believe fidelity to original meaning is the correct interpretive method and that it happens to preserve founding-class supremacy as a side effect of principle. From the enslaved and excluded seats, the constraint is pure extraction covered by a legitimacy story. From the judicial originalist seat, it is authority-grounding and interpretive method — a professional commitment to a correct reading. From the universalist seat, it is a suppression mechanism disguised as principle. The engine computes each seat's classification from the structural data (power, exit, beneficiary/victim status); the widest gap should appear between the founding-elite (beneficiary, arbitrage exit) and the enslaved/victims (powerless, trapped) seats. Originalists may compute this as rope (real coordination via stable constitutional meaning); victims compute it as snare (pure extraction with no escape). Both computations follow from the same structural data, revealing the constraint's true asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding elite and originalist judges: d → 0.0 (beneficiary end). They set the rule, enforce it, and benefit from it. Escape is available to them in principle but unnecessary — the rule serves their interests. Constitutional conservative coalition: d ≈ 0.2–0.3 (beneficiary-leaning). They benefit from constraint's narrow equality definition but are not the primary enforcers; they have mobile exit and choose to maintain the frame for strategic reasons. Women, indigenous peoples, propertyless white men: d ≈ 0.7–0.8 (target-leaning). They are bound by a rule they did not set, cannot easily exit (constrained, identity-locked to marginalized positions), and bear the cost of exclusion from the text's promised equality. Enslaved people and descendants: d → 1.0 (full target end). Trapped by a taxonomy authored before their arrival, powerless to change it without legal overthrow of the entire constraint, bearing centuries of extraction. Universalist interpreters: d ≈ 0.5 (analytical symmetric). They can articulate the contradiction without bearing its cost; they are excluded but not victimized. No directionality overrides needed — the derivation from beneficiary/victim + power + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (whether universal language can be coherently read as bound by founder intent) is contested and arguably dead: modern originalists acknowledge that founders intended slavery and women's subordination as binding categories, yet originalism cannot systematically extend to endorsing those outcomes today without losing legitimacy. The constraint persists (disappearance_verdict: world_rearranges) despite this mandatrophy because it serves the conservative coalition's distributional interests. The theater_ratio increase from 0.18 to 0.42 indicates rising performative maintenance: originalists invest more energy defending the method's coherence as the paradox becomes visible; they argue in good faith that the method is neutral and principle-driven, but the suppression of textualist-paradox critics and universalist readings is increasingly theatrical. The constraint does not qualify as piton (completely atrophied) because it retains real judicial enforcement power and produces real exclusions — it is not yet inert. But it shows mandatrophy signals: the founding problem that motivated it (instability without founder-intent constraint) is disputed; alternatives (universalism, living constitutionalism) are suppressed more by institutional power than by the method's own force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_textual_paradox,
    'How can the text''s universal language (''all men are created equal'') be coherently read as bound by the founders'' restricted application (18th-century social taxonomy) without invoking an extra-textual principle that contradicts originalism''s own stated method?',
    'Originalist scholars must either (a) provide a textualist account of how universal language can mean restricted meaning without additional extrinsic context, or (b) acknowledge that originalism relies on extra-textual founder intent rather than the text itself, which undermines the textualist foundation. Textualist critics document the logical gap; originalists either address it or suppress it.',
    'If the paradox cannot be resolved without invoking non-originalist methods, originalism''s coherence collapses and the constraint''s authority is grounded in political preference, not interpretive principle. If resolved, the resolution reveals that originalism is selective about which aspects of text vs. intent bind the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_textual_paradox, conceptual, 'Internal contradiction between universal text and restricted originalist application.').

omega_variable(
    founders_intent_coverage_gap,
    'What did the founders intend regarding persons not contemplated in their 18th-century taxonomy (future generations, persons not yet born, groups the founders'' taxonomy did not recognize as human)? Can originalism coherently extend to unanticipated persons without violating its own method?',
    'Originalist theory must specify a rule for applying 18th-century intent to persons the founders did not contemplate. Either the rule is principled (in which case it is not originalism as traditionally defined) or it is ad-hoc (in which case originalism is unstable). Historical study of what the founders would have intended regarding non-contemplated groups; comparison with how originalism handles new technologies and unanticipated scenarios.',
    'A coherent rule enables originalism to extend to new cases consistently; its absence means originalism cannot be applied universally and must resort to non-originalist bridging at crucial moments, revealing the constraint as selective in its application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founders_intent_coverage_gap, empirical, 'Whether originalism can extend to unanticipated persons without collapsing into ad-hoc reasoning.').

omega_variable(
    suppression_mechanism_identity_fusion,
    'Is the suppression that keeps alternative equality readings out of constitutional discourse structural (originalism''s arguments logically defeat alternatives), internalized (judges and scholars have fused their professional identity with originalism, making exit unthinkable), or both?',
    'Post-originalism intellectual exit and career trajectories: if scholars who leave originalism retain their standing and influence, suppression is partly structural. If they lose authority and face professional isolation, suppression is substantially internalized. Internal court dynamics: do originalist justices suppress alternative reasoning through institutional mechanisms (refusing to engage, dismissing non-originalist arguments as illegitimate) or through argumentation? Survey evidence from legal scholars on whether they adopted originalism because it persuaded them or because it became professionally required.',
    'Structural suppression means the constraint persists through argument; internalized suppression means it persists through identity lock, and post-originalism trajectory would reveal the constraint''s theatrical elements. A mixed mechanism suggests originalism is sustained by both intellectual capture and professional incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_fusion, empirical, 'Mechanism of suppression: structural argument vs. identity-locked professional identity.').

omega_variable(
    kernel_reading_relationship_to_universalist,
    'This constraint (originalist reading) claims to bind meaning to 18th-century founder intent. The universalist reading claims the same text requires iterative expansion. Are these readings logically foreclosed by each other (can no single framework hold both), or do they coexist as different commitments held by different factions?',
    'Examine whether an originalist could coherently hold that the founders intended to bind their meaning forever (originalist claim) while also holding that the text''s universal language requires it to grow (universalist claim). If coherence is impossible, the readings foreclose each other. If different institutions, factions, or parties hold them independently without internal contradiction, they coexist.',
    'Foreclosure means one reading must be chosen; coexistence means the kernel contest is irresolvable within the current constitutional framework and requires amendment to settle. The distinction determines whether the constitutional system can accommodate both readings or whether one must eliminate the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_to_universalist, conceptual, 'Whether originalist and universalist readings of equality are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(all__tr_t0, projected).
narrative_ontology:measurement(all__tr_t6, all_men_created_equal__originalist_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(all__tr_t6, observed).
narrative_ontology:measurement(all__tr_t12, all_men_created_equal__originalist_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(all__tr_t12, observed).
narrative_ontology:measurement(all__tr_t18, all_men_created_equal__originalist_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(all__tr_t18, observed).
narrative_ontology:measurement(all__tr_t24, all_men_created_equal__originalist_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(all__tr_t24, observed).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__originalist_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(all__tr_t30, observed).
narrative_ontology:measurement(all__tr_t46, all_men_created_equal__originalist_reading, theater_ratio, 46, 0.42).
narrative_ontology:measurement_basis(all__tr_t46, projected).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(all__be_t0, projected).
narrative_ontology:measurement(all__be_t6, all_men_created_equal__originalist_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(all__be_t6, observed).
narrative_ontology:measurement(all__be_t12, all_men_created_equal__originalist_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(all__be_t12, observed).
narrative_ontology:measurement(all__be_t18, all_men_created_equal__originalist_reading, base_extractiveness, 18, 0.78).
narrative_ontology:measurement_basis(all__be_t18, observed).
narrative_ontology:measurement(all__be_t24, all_men_created_equal__originalist_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement_basis(all__be_t24, observed).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__originalist_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(all__be_t30, observed).
narrative_ontology:measurement(all__be_t46, all_men_created_equal__originalist_reading, base_extractiveness, 46, 0.81).
narrative_ontology:measurement_basis(all__be_t46, projected).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(all__su_t0, projected).
narrative_ontology:measurement(all__su_t6, all_men_created_equal__originalist_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(all__su_t6, observed).
narrative_ontology:measurement(all__su_t12, all_men_created_equal__originalist_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(all__su_t12, observed).
narrative_ontology:measurement(all__su_t18, all_men_created_equal__originalist_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement_basis(all__su_t18, observed).
narrative_ontology:measurement(all__su_t24, all_men_created_equal__originalist_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement_basis(all__su_t24, observed).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__originalist_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(all__su_t30, observed).
narrative_ontology:measurement(all__su_t46, all_men_created_equal__originalist_reading, suppression_requirement, 46, 0.77).
narrative_ontology:measurement_basis(all__su_t46, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'all_men_created_equal'. The originalist reading (this story) binds meaning to 18th-century founder intent, producing high extractiveness for founding-class descendants and beneficiaries. The universalist reading treats the text as universal and expansive, computing as tangled_rope (coordination with universalist obligation vs. extraction from founders' actual restrictions). The textualist paradox reading highlights the internal contradiction between universal language and restricted application, computing as snare (pure cover story for preservation of hierarchy). All three readings are linked via network.affects_constraints to show kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
