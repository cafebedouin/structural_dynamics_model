% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative-Use Right Enabling Cultural Production
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   Fair use under U.S. copyright law (17 U.S.C. § 107) has been read by
 *   courts and scholars in structurally distinct ways since the 1990s,
 *   especially following Campbell v. Acuff-Rose Music (1994) and its progeny
 *   through Google LLC v. Oracle America (2021) and the ongoing generative-AI
 *   training-data litigation. This story authors the transformative-right
 *   reading: fair use is a right-like doctrine whose purpose is to enable
 *   commentary, criticism, parody, and follow-on innovation, and whose
 *   central analytical engine is the first statutory factor (purpose and
 *   character of the use), with the fourth factor (market effect) explicitly
 *   subordinated when transformation is found. Under this reading, the
 *   existence of a plausible licensing market does not defeat fair use — a
 *   position that directly displaces licensing intermediaries' revenue model
 *   and imposes real, uncompensated costs on rightsholders whose works are
 *   transformed without payment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.22).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.35).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative-Use Right Enabling Cultural Production").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '080f45e4-e6b5-42cd-98b6-62e8a432839e').
narrative_ontology:cs_kernel_codification('080f45e4-e6b5-42cd-98b6-62e8a432839e', fixed_text).
narrative_ontology:cs_authority_grounding('080f45e4-e6b5-42cd-98b6-62e8a432839e', lineage).
narrative_ontology:cs_interpretation_layer_present('080f45e4-e6b5-42cd-98b6-62e8a432839e').
narrative_ontology:cs_reading_relation('080f45e4-e6b5-42cd-98b6-62e8a432839e', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('080f45e4-e6b5-42cd-98b6-62e8a432839e', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('080f45e4-e6b5-42cd-98b6-62e8a432839e', foundational, transformativeness_is_the_central_inquiry).
narrative_ontology:cs_axiom_status(transformativeness_is_the_central_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('080f45e4-e6b5-42cd-98b6-62e8a432839e', transformativeness_is_the_central_inquiry, instrumental).
narrative_ontology:cs_axiom('080f45e4-e6b5-42cd-98b6-62e8a432839e', foundational, market_harm_not_dispositive_when_transformative).
narrative_ontology:cs_axiom_status(market_harm_not_dispositive_when_transformative, holdable).
narrative_ontology:cs_axiom_grounding('080f45e4-e6b5-42cd-98b6-62e8a432839e', market_harm_not_dispositive_when_transformative, instrumental).
narrative_ontology:cs_reference_frame('080f45e4-e6b5-42cd-98b6-62e8a432839e', campbell_transformativeness_primacy).
narrative_ontology:cs_drift_state('080f45e4-e6b5-42cd-98b6-62e8a432839e', post_google_v_oracle_ai_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('080f45e4-e6b5-42cd-98b6-62e8a432839e', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, commentators_and_critics).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, software_reverse_engineers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, the_public_domain_of_discourse).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, rightsholders_of_source_works).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_commons_users).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, first_amendment_breathing_space_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, progress_clause_purpose_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parodists, remix artists, appropriation artists, and commentators who repurpose existing works to add new meaning or message. Under this reading, courts examine whether their use adds new expression, meaning, or message rather than whether a license could theoretically have been purchased. They benefit from a doctrine that treats transformation itself as the central inquiry, giving them room to build on prior work without clearing rights first.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, national).

% Use archival footage, news clips, and copyrighted materials to construct historical or critical narratives. Rely on courts reading fair use generously toward purposes that inform or critique rather than substitute for the original market. Face real risk if the reading narrows, since clearance costs for archival footage can be prohibitive.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Engineers and firms who decompile or interoperate with existing software to build compatible products. Depend on courts treating interoperability-driven copying as transformative and innovation-enabling rather than as a substitute sale foreclosed by licensing potential.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, software_reverse_engineers, beneficiary,
    organized, generational, constrained, global).

% Authors, studios, and publishers whose works are reused without payment when a court finds the reuse transformative. Bear the cost of foregone licensing revenue in cases where, under a market-licensing reading, they would have been paid. Their exit options are constrained to litigation, since the doctrine's application is decided case-by-case by courts they do not control.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, rightsholders_of_source_works, payer,
    powerful, generational, constrained, global).

% Rights-clearance agencies and collective licensing bodies whose business model depends on the market-licensing view that any licensable use should be paid for. A generous transformative-use doctrine shrinks the pool of uses that must clear through them, directly reducing their transaction volume and revenue.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, payer,
    organized, biographical, constrained, national).

% Adjudicate fair use case by case under the four statutory factors, with this reading directing them to weight the first factor (purpose and character, especially transformativeness) as doing most of the analytical work, and to treat the fourth factor (market harm) as not dispositive when transformation is present. Courts administer and can shift the doctrine's center of gravity through successive rulings.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% The diffuse public that benefits from a richer, more heavily annotated, critiqued, and remixed cultural record, but has no seat in any individual fair use litigation. They gain when courts protect transformative reuse broadly but cannot advocate for themselves in the adversarial proceedings that actually set precedent.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_commons_users, beneficiary,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, cultural_commons_users, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fair use under this reading solves the problem that a strict permission requirement for every reuse of copyrighted expression would freeze commentary, parody, criticism, and follow-on innovation that depends on engaging directly with existing works, because transaction costs and holdout rightsholders would make much of that engagement impossible to license at any price.
% TRANSFER_FUNCTION: Moves permission-to-use away from rightsholders (who would otherwise capture licensing fees or block the use entirely) toward transformative users, and moves the associated economic value from potential licensing revenue to newly created transformative works and the public discourse they enable.
% ABSENT_VOICES: Individual audience members and the diffuse public who benefit from a richer cultural commons have no standing to appear in fair use litigation, which is conducted entirely between the rightsholder-plaintiff and the user-defendant; their interest in access is represented, if at all, only indirectly through amicus briefs or the court's own policy reasoning.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished and courts instead required licensability to be dispositive (the market-licensing reading), large swaths of documentary filmmaking, criticism, parody, and software interoperability work would become legally exposed overnight; rightsholders would gain leverage to demand payment or block use, licensing intermediaries would gain volume, and much marginal transformative production would either not occur or would move underground.
% FOUNDING_PROBLEM: Copyright's exclusive rights, applied literally, would let rightsholders veto or price out commentary, criticism, and follow-on creativity that copyright's own constitutional purpose (promoting progress) is supposed to encourage; fair use was built as the safety valve that keeps copyright from swallowing the expressive and innovative activity it exists to promote.
% FOUNDING_PROBLEM_CORROBORATION: First Amendment scholars and antitrust-adjacent competition economists outside the direct beneficiary class (transformative creators) corroborate that the underlying free-expression and innovation-diffusion problem remains live, citing ongoing disputes over AI training data, sampling, and software interoperability. Rightsholder trade associations dispute this, arguing that modern licensing technology (collective licensing platforms, micro-licensing markets) has solved the transaction-cost problem the doctrine was built for, making the continued broad reading a subsidy rather than a necessity — that dispute is itself evidence the founding problem's live/dead status is unresolved rather than settled in either direction.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.22) because, BY THIS READING'S OWN LIGHTS, transformative use is the doctrine functioning as intended — value is created (new expression, criticism, innovation) rather than merely transferred, and any loss to rightsholders is the necessary cost of preserving the coordination function copyright's progress clause exists to serve. Suppression is moderate (0.35): the doctrine does foreclose rightsholders' ability to demand payment for a defined category of uses, and that foreclosure is actively defended in litigation, but it does not suppress rightsholders from using or licensing their own works elsewhere. Theater ratio is low (0.2) reflecting that courts applying this reading generally engage in substantive transformativeness analysis rather than pretextual box-checking, though the ratio rises slowly over the measured interval as `transformative` gets invoked in increasingly attenuated contexts (thumbnail search results, AI training corpora) that stretch the term's original core meaning — a mild Goodhart-drift signal worth tracking, not yet severe.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, documentary filmmakers, and reverse engineers are structural beneficiaries: the doctrine subsidizes their activity by removing a permission requirement they would otherwise face, so their derived directionality sits near the beneficiary end. Rightsholders and licensing intermediaries are structural payers: value that would flow to them under a market-licensing reading is redirected to users, so their directionality sits nearer the target end, amplified by their inability to prevent any individual court from applying the doctrine (constrained exit, no unilateral opt-out). Federal courts sit in the agenda-setter seat with analytical exit — they administer and can shift the doctrine's weighting through case law, which is precisely the site of contest between this reading and its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that literal copyright exclusivity would let rightsholders veto commentary and follow-on creation — is genuinely contested rather than dead: the rise of collective licensing platforms and machine-tractable rights-clearance technology gives the market_licensing_reading's proponents a live argument that transaction costs (the original justification) have fallen, while the transformative_right_reading's proponents point to categorically new uses (AI training, large-scale digitization) where no plausible licensing market has ever existed, keeping the founding problem alive in a different form. Classifying this as a rope rather than a snare or tangled_rope prevents mislabeling a working coordination doctrine — one that lets cultural production continue without an unworkable universal permission requirement — as pure extraction merely because rightsholders bear a real, identifiable, uncompensated cost; the doctrine's defenders would say that cost is precisely the price copyright's constitutional purpose requires someone to bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the transformative_right_reading, the narrow_defense_reading, or the market_licensing_reading the legally correct account of what fair use IS, or are all three live, contest-dependent readings that different courts and circuits apply inconsistently?',
    'Track circuit split resolution and Supreme Court fair-use jurisprudence (Campbell, Google v. Oracle, Warhol v. Goldsmith) for convergence toward one reading; a durable circuit split or repeated fact-specific balancing without doctrinal convergence would indicate the three readings coexist as genuinely contested framings rather than one being simply correct.',
    'If courts converge on the market_licensing_reading, this story''s low ε for transformative use would need re-authoring upward, since the doctrine''s practical scope would narrow substantially; if courts entrench the transformative_right_reading, ε stays low and stable as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading actually governs fair use adjudication is itself contested and shifting.').

omega_variable(
    transformativeness_scope_creep,
    'Has the definition of ''transformative'' stretched beyond commentary/criticism/parody into any use that serves a different technological purpose (search indexing, AI training), and if so, does that stretch dilute the doctrine''s coordination function into something closer to a general-purpose licensing bypass?',
    'Compare judicial transformativeness findings across the interval for functional-purpose transformation (Google Books, HathiTrust) versus expressive transformation (Campbell-style parody); a rising share of functional-purpose findings with declining engagement of the expressive-meaning inquiry would indicate scope creep.',
    'If transformativeness has become untethered from genuine expressive re-creation, the rising theater_ratio in this story''s measurements would be understating a more serious Goodhart-drift problem, and rightsholders'' extraction claim strengthens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_scope_creep, empirical, 'Whether the transformative-use category has expanded past its original expressive-purpose core.').

omega_variable(
    generative_ai_training_data_stress_test,
    'Does mass ingestion of copyrighted works to train generative AI models qualify as transformative use under this reading''s own logic, or does its market-substitutive effect (competing directly with the works it was trained on) place it outside even a generous transformative-right reading?',
    'Outcome of pending AI training-data litigation (e.g., authors'' guild suits against AI developers); a finding that mass ingestion is transformative would stress-test whether this reading''s low-ε claim holds at unprecedented scale, while a finding against transformativeness would confirm this reading''s own internal limits.',
    'A pro-AI-developer ruling under this reading''s banner would dramatically expand the population of uses this story''s low ε applies to, likely requiring decomposition into a separate constraint story for AI-training fair use given the ε-invariance principle, since the beneficiary/victim structure and stakes differ qualitatively from parody or documentary use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generative_ai_training_data_stress_test, empirical, 'Whether large-scale AI training use fits within or breaks this reading''s transformative-use logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1994, 0.13).
narrative_ontology:measurement(fair_tr_t2006, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2006, 0.16).
narrative_ontology:measurement(fair_tr_t2015, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(fair_tr_t2021, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2021, 0.19).
narrative_ontology:measurement(fair_tr_t2026, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1976, 0.15).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1994, 0.18).
narrative_ontology:measurement(fair_be_t2006, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2006, 0.2).
narrative_ontology:measurement(fair_be_t2015, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(fair_be_t2021, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2021, 0.23).
narrative_ontology:measurement(fair_be_t2026, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2026, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1994, 0.28).
narrative_ontology:measurement(fair_su_t2006, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2006, 0.3).
narrative_ontology:measurement(fair_su_t2015, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(fair_su_t2021, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2021, 0.34).
narrative_ontology:measurement(fair_su_t2026, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the fair_use_statutory_exception kernel. The narrow_defense_reading treats fair use as a defense narrowly construed to preserve copyright's property character (higher ε for most reuse, since the presumption favors the rightsholder). The market_licensing_reading treats licensability itself as dispositive against fair use (highest ε for any use with a plausible licensing market, since almost any use can theoretically be licensed). This transformative_right_reading produces the lowest ε of the three for genuinely transformative uses, because it treats transformation — not licensability or property presumption — as the central and often sufficient inquiry. All three readings share the same underlying statutory text (17 U.S.C. § 107) and the same kernel contest; they diverge in which factor does the analytical work and in how the fourth factor (market harm) is weighted against the first.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
