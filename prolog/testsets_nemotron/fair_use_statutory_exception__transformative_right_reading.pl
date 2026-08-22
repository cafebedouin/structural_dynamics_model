% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Fair Use Statutory Exception — Transformative Right Reading
 *   domain: intellectual_property/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the transformative_right_reading of
 *   the fair_use_statutory_exception kernel. The reading holds that fair use
 *   is an affirmative right enabling transformative reuse and cultural
 *   production, not merely a narrow defense against infringement claims.
 *   Courts are structurally positioned as facilitators of innovation: the
 *   four statutory factors are balanced with a thumb on the scale for
 *   transformative purpose, licensing markets are not dispositive, and the
 *   burden of proof is shared rather than placed entirely on the defendant.
 *   The reading's ε is low (0.18) for transformative uses because the
 *   constraint extracts little from those it governs — it enables rather than
 *   restricts. For substitutive uses (mere copying), ε would be higher, but
 *   those uses fall outside this reading's protected core. The beneficiary
 *   set is broad: transformative creators, cultural institutions, educators,
 *   researchers, journalists, and the public. The victim set is narrow:
 *   rights holders who claim substitutive uses as fair use. The reading
 *   coexists with sibling readings (narrow_defense_reading,
 *   market_licensing_reading) as live positions in judicial and scholarly
 *   discourse — no single framework forecloses the others, though they create
 *   mutual structural pressure.
 *
 * KEY AGENTS:
 *   - transformative_creators: Primary beneficiary (organized/constrained) — enabled to build on existing culture
 *   - cultural_institutions: Primary beneficiary (institutional/constrained) — libraries, archives, museums rely on fair use for preservation and access
 *   - educational_users: Primary beneficiary (organized/constrained) — teaching and scholarship depend on flexible reuse
 *   - researchers: Primary beneficiary (organized/constrained) — text/data mining, computational analysis require fair use
 *   - journalists: Primary beneficiary (organized/constrained) — reporting and commentary require quotation and critique
 *   - general_public: Primary beneficiary (organized/mobile) — cultural participation and expression
 *   - rights_holders_claiming_substitutive_uses: Victim of this reading (powerful/constrained) — their claim to control transformative uses is denied
 *   - courts: Agenda setter (institutional/analytical) — adjudicate the boundary, set precedent
 *   - copyright_industries: Secondary payer (powerful/mobile) — licensing revenue constrained by fair use scope
 *   - legal_scholars: Observer (analytical/analytical) — analyze and critique the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.18).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.15).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use Statutory Exception — Transformative Right Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property/legal_interpretation/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e').
narrative_ontology:cs_kernel_codification('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', fixed_text).
narrative_ontology:cs_authority_grounding('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', lineage).
narrative_ontology:cs_interpretation_layer_present('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e').
narrative_ontology:cs_reading_relation('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', foundational, transformative_creation_as_distinct_economic_activity).
narrative_ontology:cs_axiom_status(transformative_creation_as_distinct_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', transformative_creation_as_distinct_economic_activity, empirically_contingent).
narrative_ontology:cs_axiom('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', foundational, shared_burden_in_fair_use_analysis).
narrative_ontology:cs_axiom_status(shared_burden_in_fair_use_analysis, holdable).
narrative_ontology:cs_axiom_grounding('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', shared_burden_in_fair_use_analysis, conventional).
narrative_ontology:cs_axiom('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', secondary, licensing_market_non_dispositive).
narrative_ontology:cs_axiom_status(licensing_market_non_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', licensing_market_non_dispositive, empirically_contingent).
narrative_ontology:cs_reference_frame('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', campbell_transformative_use_framework).
narrative_ontology:cs_drift_state('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', post_google_v_oracle_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e6ee5db8-0d96-4c2d-9bf4-3b95f3e84f2e', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_institutions).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, educational_users).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, researchers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, journalists).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, general_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, rights_holders_claiming_substitutive_uses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, rights_holders_claiming_substitutive_uses).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_industries).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, cultural_progress_clause).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, first_amendment_protection_of_expression).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, burden_sharing_in_fair_use_analysis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, musicians, filmmakers, and digital creators who build new works by transforming existing copyrighted material — parody, criticism, commentary, remix, collage, appropriation art, sampling. They rely on fair use to create without licensing negotiation or fee payment. Their exit is constrained: they cannot easily create equivalent work without referencing the cultural corpus, and licensing is often impractical (orphan works, refusal, cost). They benefit directly from the low-extraction regime this reading establishes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    organized, biographical, constrained, national).

% Libraries, archives, museums, and universities that preserve, provide access to, and enable research on copyrighted collections. They depend on fair use for digitization, preservation copying, text and data mining, exhibition, and educational use. Their exit is constrained by mission and public mandate — they cannot simply avoid copyrighted material. They are institutional beneficiaries who also serve the general public.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_institutions, beneficiary,
    institutional, generational, constrained, national).

% Teachers, students, and educational institutions using copyrighted material for teaching, scholarship, and research. Classroom copying, course reserves, distance learning, and student projects all rely on fair use. Exit is constrained by curriculum requirements and the necessity of engaging with the cultural canon. They benefit from the reading's expansive view of educational purpose.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, educational_users, beneficiary,
    organized, biographical, constrained, national).

% Academic and independent researchers conducting text and data mining, computational analysis, systematic reviews, and other research requiring large-scale use of copyrighted works. Fair use enables research that would be impossible under a permission regime. Their exit is constrained by the nature of research questions — they must study what exists. They are beneficiaries of the reading's recognition of research as transformative purpose.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, researchers, beneficiary,
    organized, biographical, constrained, global).

% Reporters, editors, and news organizations using copyrighted material for reporting, criticism, commentary, and news gathering. Quotation, screenshot, clip, and document reproduction are essential to journalism. Exit is constrained by the news cycle and public interest — they cannot wait for permission. They benefit from the reading's protection of reporting and commentary as core transformative purposes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, journalists, beneficiary,
    organized, biographical, constrained, national).

% Ordinary citizens engaging in cultural participation: memes, fan fiction, home videos, social media sharing, personal study. They benefit from a cultural commons enriched by transformative reuse. Their exit is relatively mobile — they can consume rather than create, use public domain works, or seek licensed alternatives — but the cultural environment they inhabit is shaped by the constraint's operation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Copyright owners (individual authors, publishers, studios, record labels) who assert control over uses they characterize as substitutive — verbatim copying, format shifting, or derivatives that serve the same market function as the original. Under this reading, their claims to control transformative uses are denied; they bear the cost of lost licensing revenue for uses deemed fair. They are also secondary beneficiaries when the reading protects their own transformative uses of others' works. Their exit is constrained: they cannot opt out of the fair use exception, but they retain full control over non-transformative licensing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, rights_holders_claiming_substitutive_uses, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, rights_holders_claiming_substitutive_uses, beneficiary).

% Federal courts (especially Courts of Appeals and the Supreme Court) that adjudicate fair use disputes and set binding precedent. They administer the constraint by applying the four-factor test with the reading's interpretive gloss: transformative purpose favored, licensing markets not dispositive, burden shared. They bear adjudication costs but gain legitimacy from facilitating cultural progress. Their exit is analytical — they interpret the constraint but do not bear its extraction or benefit from its coordination in the same way parties do.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Major content industries (film, music, publishing, software) that license derivative works and view fair use as a constraint on their licensing revenue. They lobby for narrower interpretations, fund litigation, and develop technological controls (DRM, content ID). They are payers under this reading because the expansive transformative right reduces their licensing base. Their exit is relatively mobile — they can adapt business models, focus on non-transformative licensing, or advocate for legislative change — but they bear significant transition costs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_industries, payer,
    powerful, generational, mobile, global).

% Academics, practitioners, and policy analysts who study, critique, and propose reforms to fair use doctrine. They neither collect rents nor pay them directly; they observe the constraint's operation across seats and contribute to the interpretive discourse that shapes judicial application. Their exit is analytical — they engage by choice and can shift focus to other doctrines.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cumulative innovation problem: cultural production builds on prior culture; requiring permission for every transformative reuse would create prohibitive transaction costs, holdout problems, and chilling effects. Fair use as a transformative right provides a default permission structure that enables follow-on creativity without central coordination.
% TRANSFER_FUNCTION: Moves control over transformative reuse from rights holders to follow-on creators. Rights holders lose the ability to license or veto transformative uses; creators gain the freedom to transform without payment. The transfer is not monetary but allocative — decision rights over cultural building blocks shift from originators to transformers.
% ABSENT_VOICES: Future creators whose transformative uses have not yet been litigated — they would benefit from the reading's expansive scope but are not present in current cases. Also absent: rights holders in jurisdictions without fair use (most of the world) who would object to the U.S. model's breadth but have no standing in U.S. courts. The excluded role is partially filled by rights_holders_claiming_substitutive_uses, but the global perspective is missing.
% DISAPPEARANCE_RATIONALE: If the transformative right reading vanished overnight, the default would shift to permission-based culture: transformative creators would need licenses for every reference, quotation, sample, or critique. Cultural production would slow, concentrate in well-resourced entities, and avoid critical or parodic engagement with dominant works. Libraries and archives would curtail digitization. Research would face prohibitive licensing. The cultural ecosystem would reorganize around permission and payment.
% FOUNDING_PROBLEM: The constitutional mandate to 'promote the Progress of Science and useful Arts' (Art. I, Sec. 8, Cl. 8) requires a copyright system that enables, not merely rewards, cultural production. The founding problem of fair use is that exclusive rights, if absolute, would block the very progress they are meant to incentivize — follow-on creators cannot build on a culture they cannot touch. Fair use was built (judicially, then codified in 1976) to ensure the copyright system does not eat its own seed corn.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional progress clause and First Amendment are cited by courts (Campbell, Google v. Oracle) and scholars (Litman, Lessig, Samuelson, Tushnet) outside the immediate beneficiary set. Economic studies of cumulative innovation (Scotchmer, Green & Scotchmer) corroborate that follow-on innovation requires access. The CONTU report (1978) and legislative history of the 1976 Act show Congress understood fair use as a safety valve for progress. However, rights holder groups (RIAA, MPAA, AAP) contest that the founding problem is solved by voluntary licensing and that fair use has expanded beyond its original justification.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint primarily enables activity rather than extracting from it. The reading's core axiom — transformative creation is a distinct economic activity that does not merely substitute for the original — means the constraint imposes minimal cost on its beneficiaries. Suppression is low (0.15) because the constraint does not require active enforcement against alternatives; it operates as a permissive rule. Theater is low (0.12) because the doctrine's operation tracks its stated purpose: courts do not perform transformative analysis while actually applying market substitution. Accessibility collapse is modest (0.25) because alternative frameworks (licensing, public domain, permission) remain available — the constraint does not foreclose other paths. Resistance is moderate (0.55) because rights holders and their industries actively litigate to narrow the doctrine, and courts sometimes drift toward market_licensing_reading frameworks. The measurement series shows declining extraction and suppression from 1994 (pre-Campbell) to 2024, reflecting the reading's consolidation after Campbell v. Acuff-Rose (1994) and its subsequent institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   From the transformative creator's seat (beneficiary, organized, constrained exit), the constraint is a genuine rope: it solves the coordination problem of building on culture without permission, with minimal coercive overhead. From the rights holder's seat (powerful, constrained exit when their substitutive claims are denied), the same constraint operates as extraction — they lose control over derivative markets. From the court's seat (agenda setter, institutional, analytical), the constraint is a coordination mechanism with genuine interpretive difficulty at the boundaries. The engine computes these divergences from the structural data; the claimed_type (rope) reflects the reading's self-understanding as a coordination mechanism, while the metrics reflect its actual operation across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transformative_creators, cultural_institutions, educational_users, researchers, journalists, general_public) receive low directionality values (d ~ 0.1-0.2): the constraint subsidizes their activity. The victim group (rights_holders_claiming_substitutive_uses) receives high directionality (d ~ 0.8): the constraint denies them control they would otherwise exercise. Courts as agenda_setters sit near symmetric (d ~ 0.5): they bear adjudication costs but gain institutional legitimacy from facilitating cultural progress. Copyright industries as secondary payers sit at d ~ 0.4: they lose some licensing revenue but gain a stable, predictable doctrine. The general_public has the lowest d (arbitrage-grade exit via public domain, licensing, original creation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling cultural progress by permitting transformative reuse without permission — remains live (founding_problem_status: contested). The reading's mandate has not atrophied; if anything, digital culture has expanded the domain of transformative use. However, the contestation is real: rights holders argue the founding problem is solved by licensing markets (market_licensing_reading) or that the property right should predominate (narrow_defense_reading). The corroboration from outside the beneficiary set (legislative history, First Amendment jurisprudence, economic studies of cumulative innovation) supports the reading's continued relevance. No mandatrophy resolution is declared — the constraint's function remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_vs_substitutive_boundary,
    'Where is the structural boundary between transformative and substitutive use — and does the reading''s own framework locate it in the work''s new expression or in the economic effect on the original?',
    'Case-by-case judicial application tracked against the reading''s stated axiom (transformative_creation_as_distinct_economic_activity); if courts consistently require economic substitution analysis as a proxy for transformation, the boundary has drifted toward the market_licensing_reading''s framework.',
    'If the boundary collapses to economic substitution, this reading''s low ε for transformative uses becomes unstable — extractiveness rises for edge cases and the reading converges toward narrow_defense_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_substitutive_boundary, conceptual, 'Whether transformative/substitutive is a formal distinction in the work or an economic proxy').

omega_variable(
    licensing_market_endogeneity,
    'Does the existence of a licensing market for a use type structurally preclude fair use (market_licensing_reading) or is the market endogenous to the fair use doctrine''s scope (transformative_right_reading)?',
    'Observe whether courts treat licensing availability as dispositive (market_licensing_reading) or as one factor that does not foreclose fair use (this reading). Track Campbell v. Acuff-Rose and subsequent circuit splits.',
    'If licensing markets become dispositive, ε rises for all uses with potential markets — the reading''s low-extraction core shrinks. This is the primary structural delta from the market_licensing_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_endogeneity, empirical, 'Whether licensing market existence forecloses fair use or is treated as non-dispositive').

omega_variable(
    kernel_reading_fair_use_transformative_right,
    'This constraint is the transformative_right_reading of the fair_use_statutory_exception kernel. What structural elements would change under the narrow_defense_reading and market_licensing_reading siblings?',
    'Compare beneficiary/victim sets, extractiveness profiles, and burden allocations across the three readings. The narrow_defense_reading shifts beneficiaries toward rights_holders and raises ε; the market_licensing_reading makes licensing market existence dispositive, raising ε for any use with a potential market.',
    'Sibling readings instantiate different constraints with different ε, different beneficiary/victim structures, and different claimed_types. The kernel_id is fair_use_statutory_exception; this reading_id is transformative_right_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_fair_use_transformative_right, conceptual, 'Commitment-system framing: this reading of the fair use kernel and its structural differences from sibling readings').

omega_variable(
    burden_of_proof_allocation,
    'Does the reading''s ''shared burden'' axiom mean the defendant bears initial production burden but the plaintiff bears ultimate persuasion burden, or is the burden genuinely symmetric across factors?',
    'Track judicial opinions for explicit burden allocation language: ''defendant must prove transformative purpose'' vs. ''plaintiff must prove market harm'' vs. ''holistic balancing''. The reading''s axiom names ''shared_burden_in_fair_use_analysis'' but the operational allocation determines effective extraction for defendants with fewer resources.',
    'If burden falls disproportionately on defendants, ε effectively rises for resource-constrained creators — the reading''s low-extraction promise holds only for well-resourced parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, empirical, 'Operational burden allocation within the ''shared burden'' axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_trr_tr_t1994, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(fair_use_trr_tr_t2004, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(fair_use_trr_tr_t2014, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(fair_use_trr_tr_t2024, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(fair_use_trr_be_t1994, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1994, 0.25).
narrative_ontology:measurement(fair_use_trr_be_t2004, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2004, 0.22).
narrative_ontology:measurement(fair_use_trr_be_t2014, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2014, 0.18).
narrative_ontology:measurement(fair_use_trr_be_t2024, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_trr_su_t1994, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1994, 0.2).
narrative_ontology:measurement(fair_use_trr_su_t2004, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2004, 0.18).
narrative_ontology:measurement(fair_use_trr_su_t2014, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2014, 0.15).
narrative_ontology:measurement(fair_use_trr_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, information_standard).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.02).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act_anticircumvention).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, orphan_works_mass_digitization).

% DUAL FORMULATION NOTE:
% This reading decomposes the fair use kernel with narrow_defense_reading and market_licensing_reading. All three share the fixed_text kernel (17 USC 107) but instantiate different constraints: this reading (rope, low ε, broad beneficiaries), narrow_defense_reading (tangled_rope, moderate ε, rights_holder beneficiaries), market_licensing_reading (snare, high ε for uses with potential markets, rights_holder beneficiaries). The ε-invariance principle requires separate stories because ε differs structurally across readings — the transformative/substitutive boundary and licensing market disposition are not measurement choices but different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, powerful, 0.75).
constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
