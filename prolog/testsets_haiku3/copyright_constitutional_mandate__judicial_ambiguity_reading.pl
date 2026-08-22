% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Discretion via Judicial Deference
 *   domain: constitutional_law/intellectual_property
 *
 * SUMMARY:
 *   This reading instantiates the 'judicial ambiguity' framing of the
 *   copyright constitutional mandate kernel. The core commitment is Article
 *   I, Section 8's grant to Congress of power to set copyright duration 'for
 *   limited Times.' This reading interprets that commitment to mean Congress
 *   has broad discretion within the meaning of 'limited,' and courts should
 *   defer via rational basis review rather than police the boundary. The
 *   Sonny Bono Copyright Term Extension Act (1998) exemplifies the reading:
 *   Congress extended terms by 20 years (retroactively extending existing
 *   works), the Supreme Court upheld it in Eldred v. Ashcroft (2003) using
 *   rational basis reasoning, and the deference doctrine entrenched itself.
 *   The measurement series traces the growth of extractiveness from 1790
 *   (minimal copyright monopoly) through 1998 (CTEA passage, peak
 *   extractiveness at 0.52), with modest decline by 2026 as public backlash
 *   and creative-commons adoption create counter-pressure. Theater rises
 *   monotonically: the justification narrative ('promoting progress') becomes
 *   increasingly detached from the functional operation (extending monopolies
 *   for works already written).
 *
 * KEY AGENTS:
 *   - congressional_authority: agenda-setter, institutional power, unilateral term-setting authority
 *   - federal_judiciary: secondary beneficiary through deference, institutional power, enforces rational basis standard
 *   - copyright_dependent_industries: beneficiary, organized power, lobbies for extensions and ratification
 *   - public_domain_interests: payer (powerless), generational time horizon, trapped exit, diffuse costs
 *   - constitutional_scholars: excluded observer, moderate power, constrained by lack of standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.48).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Discretion via Judicial Deference").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional_law/intellectual_property").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '899231e6-1df5-48d9-94ce-040925e6b287').
narrative_ontology:cs_kernel_codification('899231e6-1df5-48d9-94ce-040925e6b287', fixed_text).
narrative_ontology:cs_authority_grounding('899231e6-1df5-48d9-94ce-040925e6b287', lineage).
narrative_ontology:cs_interpretation_layer_present('899231e6-1df5-48d9-94ce-040925e6b287').
narrative_ontology:cs_reading_relation('899231e6-1df5-48d9-94ce-040925e6b287', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('899231e6-1df5-48d9-94ce-040925e6b287', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('899231e6-1df5-48d9-94ce-040925e6b287', foundational, limited_times_permits_legislative_discretion).
narrative_ontology:cs_axiom_status(limited_times_permits_legislative_discretion, holdable).
narrative_ontology:cs_axiom_grounding('899231e6-1df5-48d9-94ce-040925e6b287', limited_times_permits_legislative_discretion, deontological).
narrative_ontology:cs_axiom('899231e6-1df5-48d9-94ce-040925e6b287', foundational, rational_basis_review_appropriate_copyright).
narrative_ontology:cs_axiom_status(rational_basis_review_appropriate_copyright, holdable).
narrative_ontology:cs_axiom_grounding('899231e6-1df5-48d9-94ce-040925e6b287', rational_basis_review_appropriate_copyright, instrumental).
narrative_ontology:cs_axiom('899231e6-1df5-48d9-94ce-040925e6b287', secondary, judicial_deference_preserves_separation_of_powers).
narrative_ontology:cs_axiom_status(judicial_deference_preserves_separation_of_powers, holdable).
narrative_ontology:cs_axiom_grounding('899231e6-1df5-48d9-94ce-040925e6b287', judicial_deference_preserves_separation_of_powers, deontological).
narrative_ontology:cs_reference_frame('899231e6-1df5-48d9-94ce-040925e6b287', congressional_discretion_within_textual_bounds).
narrative_ontology:cs_drift_state('899231e6-1df5-48d9-94ce-040925e6b287', contemporary_post_eldred_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('899231e6-1df5-48d9-94ce-040925e6b287', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_drift_constraint).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_dependent_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, open_source_and_creative_commons_movement).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_review_standard).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length through statute. Under this reading, Congress has broad discretion to lengthen terms so long as the statute rationally relates to promoting progress in science and useful arts. Courts defer to the legislative judgment via rational basis review. Congress collects no direct rents but gains political capital from copyright-dependent industries and avoids the risk of judicial invalidation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, analytical, national).

% Under this reading, courts are beneficiaries of the deference doctrine: they avoid having to invalidate major copyright legislation by adopting a permissive rational basis standard. This preserves institutional comity with Congress and shields the judiciary from political retaliation. Courts maintain stability by not second-guessing legislative policy judgments about optimal copyright incentives.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, observer).

% Publishers, studios, music labels, and software vendors benefit from term lengthening: their back-catalog monopolies extend, competitive entry is delayed, and their influence over copyright policy shapes outcomes in their favor. They lobby Congress directly and support judicial deference doctrine through amicus briefs in key cases. Their exit options are high (they can invest in copyright-dependent industries globally; if U.S. law becomes unfavorable, they can lobby for change or shift focus).
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_dependent_industries, beneficiary,
    organized, biographical, arbitrage, global).

% The diffuse public, future creators, educators, and cultural institutions (libraries, archives, museums) bear the cost of delayed public-domain enrichment. Works remain locked under copyright monopoly longer, educational reuse is restricted, derivative works cannot legally evolve, and the cultural commons accumulate more slowly. They have no organized political voice; their interests are diffuse, collective, and long-term. Their exit is trapped: they cannot simply stop using copyright-protected works; cultural transmission requires engaging with copyrighted material.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_interests, payer,
    powerless, generational, trapped, global).

% A subset of constitutional scholars argue that 'limited Times' sets a genuine upper boundary on copyright duration (e.g., life of author plus 30 years, or one generation), and that rational basis review is an abdication of constitutional duty. Their voices appear in law review journals, amicus briefs, and legislative testimony, but they lack legal standing to sue and are systematically excluded from the judicial reasoning that validates the deference doctrine. Their exit is constrained: they can publish objections and file amicus briefs, but cannot directly challenge the doctrine through litigation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_scholars, excluded,
    moderate, generational, constrained, national).

% Creators and platforms that bypass copyright through voluntary licensing (GPL, Creative Commons) represent an exit option that works around the constraint. They bear the cost of fragmented licensing schemes and limited interoperability, but their existence demonstrates that copyright duration is not the only possible model. Their exit options are mobile: they can shift licensing models, platform infrastructure, and advocacy strategy in response to copyright law changes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, open_source_and_creative_commons_movement, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, open_source_and_creative_commons_movement, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable legal framework for copyright policy-setting: Congress can extend terms through statute, courts will review under rational basis (ensuring a reasonable justification), and the system operates without constitutional invalidation risk. This enables long-term planning for copyright-dependent industries and avoids the friction of litigated constitutional boundaries.
% TRANSFER_FUNCTION: Transfers interpretive authority over the constitutional boundary ('limited Times') from courts (who might enforce a substantive limit) to Congress (who decides term length). The constraint moves definitional power from the judicial branch to the legislative branch. This transfer enables Congress to respond to industry lobbying by extending terms; copyright industries gain longer monopolies, and Congress gains political capital.
% ABSENT_VOICES: Constitutional scholars who read 'limited Times' as a binding, judicially enforceable constraint are excluded from the operative reasoning. Their objections appear only in scholarly literature and amicus briefs, not in the judicial opinions that constitute binding doctrine. The diffuse public-domain constituency has no organized lobbyist or representative in copyright litigation; only copyright holders and Congress substantively participate in policy-making.
% DISAPPEARANCE_RATIONALE: If judicial deference collapsed and courts began enforcing a genuine 'limited Times' constraint (via strict scrutiny or heightened review), Congress could no longer unilaterally extend copyright terms by statute. Copyright legislation would face constitutional invalidation risk, Congress would lose political leverage with copyright industries, and the public domain would grow on schedule. The institutional settlement between Congress (legislates) and courts (defers) would fracture, and copyright policy would become a site of genuine constitutional contest rather than legislative discretion.
% FOUNDING_PROBLEM: The Constitution grants Congress power to set copyright duration 'for limited Times' but does not specify what 'limited' means quantitatively. Without guidance, constitutional review of copyright statutes requires judges to make policy judgments about optimal copyright incentives. How much deference should courts give Congress on this question?
% FOUNDING_PROBLEM_CORROBORATION: Congress and copyright-dependent industries explicitly attest that the deference doctrine solves the founding problem: Congress can legislate copyright policy without fear of judicial invalidation, industries can lobby for extensions without constitutional risk, and the system operates predictably. Constitutional scholars (writing in journals and testifying in legislative hearings) attest that the founding problem has NOT been solved but rather eliminated: the original boundary ('limited Times') has been rendered unenforceable through systematic judicial abdication, so the policy question is no longer whether Congress can extend terms, but only HOW FAR. The Supreme Court's majority opinion in Eldred v. Ashcroft (2003) represents the institutional endorsement of deference; Justice Breyer's dissent represents the constitutional-scholar objection.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at present) rather than high because the reading's own framing preserves a theoretical limit ('limited Times' does mean something), and Congress has not yet claimed perpetual copyright or explicitly abandoned the 'limited' constraint as nonsense. The extractiveness increased substantially from 1790–1998 as Congress extended terms and courts consistently deferred. The recent slight decline (2015–2026) reflects growing open-source cultural resistance and the fact that marginal term extensions yield diminishing political returns for copyright industries. Theater ratio tracks the growing gap between stated justification (promoting progress) and functional operation (protecting already-written works from public domain entry): at 1790 the two were aligned; by 2026 the justification narrative has become mostly decoupled from the actual impact. Suppression requirement reflects the need for courts to actively reject constitutional challenges and for Congress to legislatively reaffirm copyright in the face of public-domain advocacy. The deference doctrine itself is the suppression mechanism: without rational basis review and the Eldred precedent, copyright statutes would face genuine constitutional risk. The analytical time-grid shared across all three metrics honors the simultaneous evolution of extractiveness, theater, and enforcement burden.
 *
 * PERSPECTIVAL GAP:
 *   A majority opinion in Eldred v. Ashcroft (2003) represents the judicial deference seat: the opinion reason that Congress has legitimate policy reasons for extending terms (providing incentive for digitization and preservation), that the term extension rationally relates to the constitutional purpose (promoting progress), and that courts are not the appropriate forum to second-guess quantitative policy judgments about how much incentive is needed. A dissent (Justice Breyer) represents the constitutional scholar seat: arguing that 'limited Times' must mean something judicially enforceable, that Congress cannot extend indefinitely, and that the majority has abdicated constitutional duty. The two opinions occupy the same courtroom but compute entirely different types.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority is the primary beneficiary (d near 0.0): Congress sets the rules, courts defer, authority is never questioned. Copyright industries are secondary beneficiaries (d near 0.1–0.2): they do not run the system but their interests directly shape congressional choices and their lobbying is rewarded. The federal judiciary is a tertiary beneficiary (d near 0.15): deference is institutionally comfortable; striking down copyright legislation would impose political costs. Public-domain interests are the victims (d near 0.95): they bear the cost of delayed public-domain enrichment, have no political voice, and face trapped exit (the constitutional boundary they would invoke is unavailable due to rational basis review). Constitutional scholars occupy a middle ground (d near 0.7): they bear the cost of having their constitutional objection systematically ignored, but as institutional actors they retain some power through amicus briefs and legislative testimony. The directionality derivation from beneficiary/victim declarations produces this spread automatically; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—copyright policy as a zone of legislative discretion—is LIVE and CONTESTED. Congress and copyright industries attest it is still live: they continue to lobby for extensions and defend them as promoting progress. Constitutional scholars attest it is functionally dead: the rational basis standard is so permissive that 'limited Times' no longer constrains Congress, so the founding problem has been solved by eliminating judicial constraint entirely. The constraint's mandatrophy status is CONTESTED but moving toward RESOLVED (the judicial deference doctrine is so entrenched that the founding problem can no longer activate legal challenge). This reading does not present a mandate-dead zombie; instead, it presents a mandate whose viability depends entirely on courts remaining willing to defer. If courts ever switched to strict scrutiny or genuine 'limited Times' review, the founding problem would immediately become live again and copyright legislation would face invalidation risk. The measurement series shows theater_ratio rising: the justification narrative has become increasingly performative as Congress extends terms without new policy justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_semantic_boundary,
    'Does ''limited Times'' have a judicially discoverable meaning that constrains Congress, or is it genuinely ambiguous and necessarily left to legislative discretion?',
    'Comparative constitutional analysis: examine how other ''limited'' temporal grants in constitutions (e.g., presidential terms, patent durations in other nations) are treated. Linguistic/philosophical analysis of what ''limited'' semantically entails. Legislative history of the copyright clause and subsequent term-extension debates.',
    'If ''limited'' has a discoverable boundary (e.g., ''not more than one generation of author life'' or ''not retroactively extended''), strict scrutiny or heightened review becomes appropriate, and this reading''s deference doctrine becomes invalid. If the term is genuinely ambiguous, rational basis review is defensible. The classification would shift from rope (coordination with deference) to snare (extraction through interpretive abdication).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_semantic_boundary, conceptual, 'Semantic content of ''limited Times'' and whether courts can judicially enforce it.').

omega_variable(
    rational_basis_pretense,
    'Does rational basis review in the copyright context function as genuine judicial review, or is it a pretense that masks judicial abdication?',
    'Systematic review of copyright decisions post-Eldred: how many copyright statutes have been struck down under rational basis review (answer: essentially none). Comparison to other domains where rational basis review operates and whether similar deference rates occur. Judicial testimony or explicit reasoning about whether the court is genuinely evaluating the statute''s rationality or merely rubber-stamping it.',
    'If rational basis review is functionally a rubber stamp, the constraint is better classified as snare (suppression through judicial capitulation) than rope (coordination through deference). The measurement of theater_ratio would move higher—the function is suppression, not coordination, dressed as deference.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_basis_pretense, empirical, 'Whether rational basis review in copyright law represents genuine judicial constraint or institutional abdication.').

omega_variable(
    congressional_escalation_mechanism,
    'Does the deference doctrine create a feedback loop where Congress, knowing it faces only rational basis review, escalates term extensions over time, and does courts'' continued deference enable that escalation?',
    'Time-series analysis of term-extension magnitude per statute (1870 Act, 1976 Act, 1998 CTEA, etc.). Measurement of whether Congress''s extensions grew larger as courts'' deference became clear. Legislative testimony about expected judicial review and how it shaped congressional behavior.',
    'If a feedback loop exists, the constraint is functioning as an extraction mechanism that masks itself as coordination: Congress extends terms, courts defer, industries lobby for the next extension, and the cycle repeats with higher stakes. The true structure is escalating extraction (snare), not stable coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_escalation_mechanism, empirical, 'Whether deference doctrine creates positive feedback toward maximal copyright expansion.').

omega_variable(
    constitution_vs_doctrine_binding,
    'Is the judicial deference (rational basis review) itself constitutionally mandated, or is it merely a judicial doctrine that courts could reverse?',
    'Originalist and living-constitution scholarship on whether rational basis review is what the Framers intended for copyright review. Examination of whether a future Court could adopt strict scrutiny or genuine ''limited Times'' review without the Constitution changing. Legislative history and constitutional convention records.',
    'If deference is merely doctrine (not constitutionally required), a future Court could shift to stricter review and invalidate term extensions. If deference IS constitutionally mandated (e.g., separation-of-powers doctrine requires it), the constraint is more entrenched. This affects the time horizon of the constraint''s stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitution_vs_doctrine_binding, conceptual, 'Whether rational basis deference in copyright is constitutionally mandatory or a reversible judicial choice.').

omega_variable(
    kernel_reading_contest,
    'Is this reading (judicial ambiguity) the dominant reading of the copyright kernel, or is it contested by sibling readings that claim equal or greater legitimacy?',
    'Survey of current constitutional scholarship on copyright (how many scholars endorse each reading). Examination of current Supreme Court majority opinion vs. dissenting views. Legislative testimony and copyright-industry vs. public-domain advocacy positions.',
    'If this reading is hegemonic (dominant and accepted by courts and Congress), it is entrenched. If it is contested (equal legitimacy with corporate_enclosure or public_scaffold readings), the constraint is fragile and could shift with a change in judicial or legislative personnel. The measurement of theater_ratio and resistance captures this contestation; a rising resistance trajectory indicates the reading is losing ground to alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether judicial ambiguity reading remains dominant or faces rising contestation from alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1790, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(copy_tr_t1870, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement(copy_tr_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.32).
narrative_ontology:measurement(copy_tr_t2015, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1790, 0.15).
narrative_ontology:measurement(copy_be_t1870, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1870, 0.22).
narrative_ontology:measurement(copy_be_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement(copy_be_t2015, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1790, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1790, 0.18).
narrative_ontology:measurement(copy_su_t1870, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1870, 0.24).
narrative_ontology:measurement(copy_su_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.48).
narrative_ontology:measurement(copy_su_t2015, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-constraint family decomposing the copyright_constitutional_mandate kernel. The corporate_enclosure_reading interprets 'limited Times' permissively (maximal extension). The public_scaffold_reading interprets 'limited Times' as binding constraint (generational limit). The judicial_ambiguity_reading (this story) treats 'limited' as semantically ambiguous, requiring judicial deference. Each reading has a different epsilon and beneficiary structure; they are not perspectives on a single constraint but separate constraints arising from the same kernel. The family is linked via network.affects_constraints because each reading's classification (and its measured extraction) depends partly on which sibling readings are currently held by other factions. The judicial_ambiguity reading influences both siblings: it is the current dominant reading (per Eldred), so it sets the status quo against which corporate and public readings are measured as deviations. All three stories share the same kernel_id and the fixed text (Article I, Section 8) but diverge in how they interpret 'limited Times.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
