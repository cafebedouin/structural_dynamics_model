% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Positivist Validity Criterion for US Constitutional Meaning
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_meaning: the positivist reading, on which constitutional
 *   validity derives from formal enactment procedures and institutional
 *   authority rather than external moral principles. As an operative
 *   constraint on American constitutional adjudication, it coordinates (a
 *   determinate validity test that ends legitimacy disputes case-by-case)
 *   while extracting (claims lacking textual anchors fail categorically, and
 *   the sanctioned repair path — Article V — has been effectively dormant
 *   since 1992). Assumptions stated: the interval opens at Brown (1954), the
 *   high-water mark of moral-reasoning adjudication and the trigger for the
 *   neutrality turn; the constraint is treated as the institutionalized
 *   expectation that validity questions are settled by pedigree, enforced
 *   through appointment politics and professional discipline. The
 *   claim/metric gap is deliberate: the reading is CLAIMED as tangled_rope
 *   (my structural judgment) while the metrics independently describe
 *   substantially extractive operation with a rising trajectory — the engine
 *   computes per-seat types from the structural data; I do not reconcile
 *   claim to metrics.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and principal beneficiary (institutional/constrained) — administers the pedigree criterion, collects decision-finality and the legitimacy shield
 *   - elected_legislative_majorities: beneficiary (powerful/constrained) — enactments immune from unwritten-principle veto; holds the formal but rarely usable amendment power
 *   - textually_unsupported_rights_claimants: primary target (powerless/trapped) — bears categorical exclusion of substantively serious claims lacking textual anchors
 *   - legal_profession: secondary beneficiary (organized/identity_locked) — professional monopoly on the interpretive expertise the criterion makes decisive
 *   - natural_law_moral_philosophy_traditions: excluded voice (moderate/constrained) — defines the rival pole but holds no seat in validity determination
 *   - constitutional_scholarship: analytical observer — documents the neutrality-vs-attitudinal gap and amendment-channel viability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.74).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Positivist Validity Criterion for US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '30bfb9b3-4509-4bf0-a6ce-219f770c3947').
narrative_ontology:cs_kernel_codification('30bfb9b3-4509-4bf0-a6ce-219f770c3947', fixed_text).
narrative_ontology:cs_authority_grounding('30bfb9b3-4509-4bf0-a6ce-219f770c3947', lineage).
narrative_ontology:cs_interpretation_layer_present('30bfb9b3-4509-4bf0-a6ce-219f770c3947').
narrative_ontology:cs_reading_relation('30bfb9b3-4509-4bf0-a6ce-219f770c3947', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('30bfb9b3-4509-4bf0-a6ce-219f770c3947', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('30bfb9b3-4509-4bf0-a6ce-219f770c3947', foundational, validity_requires_enactment_pedigree).
narrative_ontology:cs_axiom_status(validity_requires_enactment_pedigree, holdable).
narrative_ontology:cs_axiom_grounding('30bfb9b3-4509-4bf0-a6ce-219f770c3947', validity_requires_enactment_pedigree, conventional).
narrative_ontology:cs_axiom('30bfb9b3-4509-4bf0-a6ce-219f770c3947', secondary, moral_reasoning_excluded_from_validity_determination).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity_determination, holdable).
narrative_ontology:cs_axiom_grounding('30bfb9b3-4509-4bf0-a6ce-219f770c3947', moral_reasoning_excluded_from_validity_determination, conventional).
narrative_ontology:cs_reference_frame('30bfb9b3-4509-4bf0-a6ce-219f770c3947', enactment_pedigree_supremacy).
narrative_ontology:cs_drift_state('30bfb9b3-4509-4bf0-a6ce-219f770c3947', contemporary_amendment_gridlock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('30bfb9b3-4509-4bf0-a6ce-219f770c3947', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, elected_legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, legal_profession).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, textually_unsupported_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which enactments count as supreme law and administers the pedigree test through doctrine: standing rules, political-question doctrine, textualist canons, precedent hierarchy. Collects decision-finality and a legitimacy shield — the ability to say it applies enacted law rather than legislating its own values. Leaving the framework would mean openly adopting moral-reasoning adjudication, which would spend accumulated institutional capital and invite court-curbing legislation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, federal_judiciary, beneficiary).

% Their enactments stand unless displaced by superior enacted law; they are insulated from judicial veto grounded in unwritten moral principle. They hold the formal power to revise the charter through the amendment process, but supermajority and state-ratification requirements make that route rarely usable, so they prefer installing sympathetic interpreters to undertaking formal revision.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, elected_legislative_majorities, beneficiary,
    powerful, biographical, constrained, national).

% Hold substantively serious grievances — political equality, representation, material security — whose resolution would require constitutional norms the enacted text does not supply. Under a pedigree-only validity test their claims fail as a category, before anyone weighs their merits. Exit means emigration or waiting on an amendment channel that has produced nothing since 1992.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, textually_unsupported_rights_claimants, payer,
    powerless, generational, trapped, national).

% Supplies the interpretive expertise the pedigree test makes decisive and staffs every institution that applies it. Professional identity is fused with a rule-of-law self-conception — law, not politics — so arguments arriving from outside the credential carry no standing. Renouncing the framework would mean renouncing the settlement that defines professional competence.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_profession, beneficiary,
    organized, biographical, identity_locked, national).

% Maintain that legal validity ultimately answers to moral truth; produce the running critique (Fuller, Dworkin, Finnis lineages) that defines the other pole of the debate. Hold no seat in validity determination: their arguments enter adjudication only after translation into textual or precedential terms, or not at all.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, natural_law_moral_philosophy_traditions, excluded,
    moderate, civilizational, constrained, global).

% Documents the gap between neutrality claims and attitudinal reality on the bench, tracks the viability of the amendment channel, and produces the empirical record on which the open questions in this story turn.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_scholarship, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, contestation-limiting standard for what counts as constitutional law: officials and citizens can identify valid constitutional requirements by checking enactment pedigree instead of litigating moral philosophy in every case. This solves a real collective-action problem — without it, every judicial review decision reopens the question of what authorizes the reviewers.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from moral-reasoning traditions and the claimants who would invoke them to formally credentialed institutions: courts gain uncontested adjudicative finality, legislative majorities gain immunity from unwritten-principle vetoes, and the costs land on those whose claims lack textual anchors.
% ABSENT_VOICES: Textually unsupported rights claimants and the natural-law traditions are structurally absent from validity determination. They would object that pedigree without justice entrenches whatever was enacted — including the enslavement-era settlements the text originally protected until amended. Their objections currently register only as political pressure on an amendment process that has been dormant since 1992.
% DISAPPEARANCE_RATIONALE: If the pedigree-only validity criterion vanished overnight, adjudication would reorganize around some alternative criterion — moral readings, evolutionary application — and everything built on the current settlement would move: confirmation politics, the professional definition of judicial competence, the finality of decided cases, and the strategic behavior of legislative majorities.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: if unelected judges strike down democratically enacted laws, what makes their rulings valid rather than usurpation? The positivist reading answers that judges merely apply enacted supreme law, and that popular control over substance runs exclusively through the amendment process.
% FOUNDING_PROBLEM_CORROBORATION: Political science on judicial legitimacy and court-curbing episodes attests the founding problem is live; rival jurisprudential traditions attest it by attacking the positivist answer rather than denying the problem; legal historians corroborate the genealogy (the neutrality turn as reaction to early-twentieth-century discretion). No source outside the dispute neutrally certifies whether the positivist answer resolves the problem — hence contested rather than live or dead.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68 at interval end: the criterion delivers genuine coordination value (predictability, democratic control over substance) while concentrating its costs on those needing extra-textual justice — and the concentration worsens as the amendment escape valve goes dormant, because the reading's own remedy for injustice becomes unavailable while the exclusion continues. Suppression 0.74: the framework cannot survive routine judicial defection, so enforcement machinery grew — ideological appointment screening, confirmation warfare, professional sanction, court-curbing threats — and the series traces that build-up deliberately. Theater 0.56: the neutrality rhetoric is increasingly performative (attitudinal models predict outcomes well; 'we merely apply the text' coexists with consequential interpretive choices), and Article V is invoked as a live remedy while functioning as none. Accessibility_collapse 0.45: alternatives remain genuinely available — living-constitutionalist adjudication recurs (Obergefell), moral arguments periodically surface — so alternatives are blunted, not erased. Resistance 0.60: sustained counter-majoritarian critique, rival-tradition attack, legitimacy crises, and periodic court-curbing proposals. All three series run on one shared seven-point grid so temporal analysis samples complete rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the target seat, the arrangement operates as categorical exclusion: claims fail on pedigree before merits, with no compensation and no exit — a snare-flavored experience. From the legislative-beneficiary seat, it is coordination they depend on: enactments stand, substance stays electorally controlled — rope-flavored. The judiciary's seat is genuinely mixed: it administers the criterion, collects finality and legitimacy from it, and simultaneously absorbs the legitimacy attacks the criterion provokes. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map onto real structural relationships. The judiciary sits near the beneficiary end: it collects the criterion's principal product (uncontested finality) and its exit is constrained by institutional identity and curbing exposure. Legislative majorities sit similarly low: they receive immunity from unwritten-principle vetoes. The legal profession is a low-to-mid beneficiary whose identity_lock amplifies persistence — it cannot cheaply renounce the settlement that defines it. Textually unsupported claimants sit at the full-target end: trapped (no jurisdictional exit), powerless, bearing the transfer in full. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope in the engine's computation. No directionality overrides were needed: the derivation from declared roles, power, and exit options captures each seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the counter-majoritarian difficulty) is contested, not dead, and disappearance would rearrange the world — so the R5 mismatch consumer finds no dead-problem/world-rearranges flag and no zombie signature. The constraint resists both mislabelings: calling it pure rope erases the categorical victim class (claimants excluded before their merits are heard); calling it pure snare erases the real coordination function (a determinate validity test that ends case-by-case legitimacy disputes). Tangled_rope holds both halves honestly. The atrophying component is specific: the 'amendment is the remedy' half has decayed under gridlock while the exclusion half persists — that partial decay is what the rising theater series records, and it is why the story watches for convergence with the originalist sibling rather than declaring the whole arrangement inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (positivist_reading) of the kernel us_constitution_meaning. What would each sibling reading change structurally if adopted as the operative criterion?',
    'Track doctrinal and appointment-pattern adoption: which validity/meaning criterion the sitting majority actually applies, and how the victim and beneficiary sets shift under each candidate criterion.',
    'Under the originalist sibling, the victim set shifts to those harmed by ratified-era meanings and the amendment-gridlock dynamic intensifies; under the living-constitutionalist sibling, the pedigree criterion dissolves and costs shift toward predictability-dependent actors. Classification of THIS story is unaffected — the siblings are separate files — but the family edges and cross-reading comparisons depend on keeping the three decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three rival readings of a single constitutional-meaning kernel; disagreement located in the validity criterion (sources thesis) versus meaning-fixation versus evolutionary content.').

omega_variable(
    positivist_originalism_collapse,
    'Does the positivist reading remain a distinct operative constraint, or has it collapsed into originalism in practice under amendment gridlock, as the expected structural delta predicts?',
    'Code positivist-authored doctrine for whether it maintains a validity criterion separable from ratification-era meaning (current-public-meaning textualism, precedent-based positivism) or converges on original-public-meaning fixation.',
    'If collapsed, this story''s effective victim set and trajectory converge toward the originalist sibling''s, the family edge strengthens, and the two stories approach redundancy; if distinct, they remain separately classified with independent epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_originalism_collapse, empirical, 'Whether the positivist and originalist readings have merged operationally under a dormant amendment channel.').

omega_variable(
    amendment_channel_viability,
    'Is Article V a live revision channel — making the positivist remedy for injustice real — or effectively closed, making ''fix it by amendment'' a theatrical invocation?',
    'Measure amendment proposals reaching congressional passage, state-application and ratification rates, and Article V convention-call activity across the interval.',
    'If closed, extraction accumulates on the trapped target seat (feeding the T17 abductive hypothesis) and theater continues rising; if revivable, the constraint retains a genuine escape valve and part of the measured extraction is the price of a working supermajoritarian safeguard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_channel_viability, empirical, 'Viability of the sanctioned repair path that legitimates the pedigree criterion''s exclusions.').

omega_variable(
    neutrality_genuineness,
    'Is the neutral-principles defense of the pedigree criterion a genuine constraint on judicial discretion, or performative cover over attitudinal decision-making?',
    'Systematic coding of constitutional rulings against attitudinal and legal-model predictions; audit whether outcomes track neutral criteria better than judges'' policy preferences alone would predict.',
    'High theater supports continued piton-drift monitoring on the maintenance dimension; demonstrated genuineness would strengthen the rope component of the tangled_rope classification and lower the justified extraction estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_genuineness, empirical, 'Whether the constraint''s central justification describes real causal work or is maintained as performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(positivist_reading_tr_t1954, us_constitution_meaning__positivist_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(positivist_reading_tr_t1966, us_constitution_meaning__positivist_reading, theater_ratio, 1966, 0.2).
narrative_ontology:measurement(positivist_reading_tr_t1978, us_constitution_meaning__positivist_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement(positivist_reading_tr_t1990, us_constitution_meaning__positivist_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(positivist_reading_tr_t2002, us_constitution_meaning__positivist_reading, theater_ratio, 2002, 0.44).
narrative_ontology:measurement(positivist_reading_tr_t2014, us_constitution_meaning__positivist_reading, theater_ratio, 2014, 0.5).
narrative_ontology:measurement(positivist_reading_tr_t2024, us_constitution_meaning__positivist_reading, theater_ratio, 2024, 0.56).

% Extraction over time
narrative_ontology:measurement(positivist_reading_be_t1954, us_constitution_meaning__positivist_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(positivist_reading_be_t1966, us_constitution_meaning__positivist_reading, base_extractiveness, 1966, 0.44).
narrative_ontology:measurement(positivist_reading_be_t1978, us_constitution_meaning__positivist_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement(positivist_reading_be_t1990, us_constitution_meaning__positivist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(positivist_reading_be_t2002, us_constitution_meaning__positivist_reading, base_extractiveness, 2002, 0.6).
narrative_ontology:measurement(positivist_reading_be_t2014, us_constitution_meaning__positivist_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(positivist_reading_be_t2024, us_constitution_meaning__positivist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(positivist_reading_su_t1954, us_constitution_meaning__positivist_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(positivist_reading_su_t1966, us_constitution_meaning__positivist_reading, suppression_requirement, 1966, 0.28).
narrative_ontology:measurement(positivist_reading_su_t1978, us_constitution_meaning__positivist_reading, suppression_requirement, 1978, 0.38).
narrative_ontology:measurement(positivist_reading_su_t1990, us_constitution_meaning__positivist_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(positivist_reading_su_t2002, us_constitution_meaning__positivist_reading, suppression_requirement, 2002, 0.58).
narrative_ontology:measurement(positivist_reading_su_t2014, us_constitution_meaning__positivist_reading, suppression_requirement, 2014, 0.66).
narrative_ontology:measurement(positivist_reading_su_t2024, us_constitution_meaning__positivist_reading, suppression_requirement, 2024, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'constitutional meaning' conflates three structurally distinct claims — where validity comes from (this story, positivist_reading), whether meaning was fixed at ratification (originalist_reading), and whether content legitimately evolves with social attitudes (living_constitutionalist_reading). Each carries its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observer-relative. Upstream/downstream structure: this reading shares pedigree commitments with the originalist sibling and exerts structural pressure on it (the collapse omega tracks convergence), while standing in logical opposition to the living-constitutionalist sibling's core premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
