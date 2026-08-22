% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA 702 Foreign Target Statutory Requirement (Strict Reading)
 *   domain: constitutional/national_security/surveillance_policy
 *
 * SUMMARY:
 *   Section 702 of FISA authorizes collection of foreign intelligence
 *   communications. The statute's 'foreign target' language restricts
 *   collection to communications where both parties and investigative
 *   interest are non-U.S. persons abroad. The strict reading of this language
 *   interprets minimization as deletion of incidental U.S. person data and
 *   categorically prohibits FBI queries of 702 collection for domestic
 *   crimes. This reading treats the statutory foreign target limit and
 *   minimization requirement as adequate protection of Fourth Amendment
 *   rights—U.S. persons retain constitutional protections through the warrant
 *   requirement for any government use of their communications, and
 *   incidental 702 data must be rendered inaccessible and deleted. The
 *   constraint is claimed as ROPE (coordination mechanism balancing foreign
 *   intelligence speed with Fourth Amendment protection) while measuring low
 *   extraction (0.15) and low theater (0.12), consistent with a
 *   rights-protective statutory interpretation. This is one reading of a
 *   contested kernel: the FISA 702 statutory text admits three incompatible
 *   readings (strict, incidental, constitutional-floor), each instantiating a
 *   different constraint with different ε values and different
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Foreign intelligence agencies (statutory budget holder, foreign target decision-maker)
 *   - FBI law enforcement (barred from domestic queries under strict reading)
 *   - U.S. persons abroad non-target (beneficiary: incidental data deleted)
 *   - Constitutional rights holders (beneficiary: warrant requirement preserved)
 *   - FISA court (enforcer: reviews foreign target certifications)
 *   - Congress (excluded: statutory author, external to constraint operation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.08).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 Foreign Target Statutory Requirement (Strict Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'a2cc974c-ebc2-47b7-947b-0908e6930cbf').
narrative_ontology:cs_kernel_codification('a2cc974c-ebc2-47b7-947b-0908e6930cbf', fixed_text).
narrative_ontology:cs_authority_grounding('a2cc974c-ebc2-47b7-947b-0908e6930cbf', lineage).
narrative_ontology:cs_interpretation_layer_present('a2cc974c-ebc2-47b7-947b-0908e6930cbf').
narrative_ontology:cs_reading_relation('a2cc974c-ebc2-47b7-947b-0908e6930cbf', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2cc974c-ebc2-47b7-947b-0908e6930cbf', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('a2cc974c-ebc2-47b7-947b-0908e6930cbf', foundational, statutory_foreign_target_language_sufficient).
narrative_ontology:cs_axiom_status(statutory_foreign_target_language_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('a2cc974c-ebc2-47b7-947b-0908e6930cbf', statutory_foreign_target_language_sufficient, conventional).
narrative_ontology:cs_axiom('a2cc974c-ebc2-47b7-947b-0908e6930cbf', foundational, minimization_as_deletion_not_access_restriction).
narrative_ontology:cs_axiom_status(minimization_as_deletion_not_access_restriction, holdable).
narrative_ontology:cs_axiom_grounding('a2cc974c-ebc2-47b7-947b-0908e6930cbf', minimization_as_deletion_not_access_restriction, deontological).
narrative_ontology:cs_reference_frame('a2cc974c-ebc2-47b7-947b-0908e6930cbf', foreign_target_statutory_protection).
narrative_ontology:cs_drift_state('a2cc974c-ebc2-47b7-947b-0908e6930cbf', contemporary_fisa_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2cc974c-ebc2-47b7-947b-0908e6930cbf', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, u_s_persons_abroad_non_target).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, u_s_constitutional_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_law_enforcement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_warrant_requirement).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_foreign_target_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Section 702 collection under the foreign target mandate: selects and monitors non-U.S. persons abroad believed to possess foreign intelligence, directs minimization procedures to delete or render inaccessible incidentally collected U.S. person communications, and reports compliance to FISA court. Operates under the strict reading's constraint that collection ceases when a target is identified as U.S. person, and U.S. person data becomes unavailable for domestic law enforcement queries absent individualized warrant.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_agencies, agenda_setter,
    institutional, generational, constrained, global).

% Under the strict reading, is categorically barred from querying the 702 database for domestic criminal investigations. Can access U.S. person incidental collection only through individual Fourth Amendment warrant process (same as pre-702 baseline). Operates under constraint that foreign intelligence and domestic law enforcement are structurally separated—the strict reading treats this separation as the statute's core mandate, not a policy preference.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_law_enforcement, payer,
    institutional, biographical, constrained, national).

% U.S. citizens or permanent residents overseas whose communications are incidentally collected when they communicate with a lawful foreign target. Under the strict reading, their data must be deleted (not merely restricted from access), and they retain Fourth Amendment protections—FBI cannot use incidental data against them domestically without warrant. The constraint protects their communications integrity by establishing minimization as deletion, not discretionary access restriction.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, u_s_persons_abroad_non_target, beneficiary,
    moderate, biographical, constrained, global).

% The general U.S. population whose Fourth Amendment rights the strict reading preserves by maintaining that warrantless search of U.S. person communications (even incidentally collected) would violate the Constitution. The constraint's beneficiary is the constitutional norm itself: that government may not bypass warrant requirement based on foreign intelligence label. Resist this interpretation; lose the constitutional floor.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, u_s_constitutional_rights_holders, beneficiary,
    organized, generational, mobile, national).

% Reviews 702 certifications under the foreign target requirement and receives compliance reports. Under the strict reading, its role is to enforce the statutory foreign target limit and ensure minimization means deletion, not mere access restriction. Can order suspension of 702 collection if targets include U.S. persons or if minimization procedures inadequately delete incidental U.S. person data.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Enacted the FISA statute and foreign target language. Under the strict reading, is the authoritative source of the constraint—the statute speaks clearly that collection targets must be non-U.S. persons abroad, and incidental U.S. person data must be minimized. Congress is not a party to the constraint's operation; it is the external source. Excluded because its voice (the statutory text) is already present in the constraint definition itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congress, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates foreign intelligence collection under a statutory foreign target requirement: designates collection targets narrowly (non-U.S. persons abroad), implements minimization procedures to delete incidental U.S. person data, and separates foreign intelligence from domestic law enforcement to preserve Fourth Amendment protections for U.S. persons.
% TRANSFER_FUNCTION: Moves the burden of obtaining individualized warrants from foreign intelligence agencies (which can collect foreign targets without warrant under 702) to domestic law enforcement (which must obtain Fourth Amendment warrant to access any U.S. person communications, including incidental 702 data). Transfers the restraint: intelligence agencies accept the foreign target limit; law enforcement accepts the warrant requirement.
% ABSENT_VOICES: Executive branch officials who interpret 702 more permissively (incidental_collection_reading seat) are present but structurally opposed; constitutional scholars arguing the foreign target language is insufficient (constitutional_floor_reading seat) are present but structurally opposed. No fully absent voice—the kernel contest is internal to the U.S. governance apparatus.
% DISAPPEARANCE_RATIONALE: If the foreign target statutory requirement and its minimization mandate vanished overnight, 702 collection would expand unconstrained to U.S. persons abroad and at home; FBI would query the database for any purpose without warrant; the Fourth Amendment protection for incidental collection would evaporate. The world would rearrange: Fourth Amendment searches would no longer require warrant whenever a foreign intelligence hook existed. Constitutional law regarding warrantless surveillance would be restructured.
% FOUNDING_PROBLEM: Post-9/11 foreign intelligence collection required fast, targeted authority to monitor foreign terrorist and espionage communications without case-by-case warrant delay; but unrestricted collection risked sweeping in U.S. persons and enabling warrantless domestic searches. The statute was written to solve both: enable foreign targeting while preserving the constitutional floor that U.S. person communications require warrant or minimization.
% FOUNDING_PROBLEM_CORROBORATION: Congressional legislative history supports the foreign target limit as a core statutory intent. Executive branch practice and intelligence agency testimony attests the foreign target requirement remains operationally binding. Constitutional scholars and civil liberties organizations outside government corroborate that the founding problem (balancing speed in foreign intelligence with Fourth Amendment protection) remains live and requires the statutory solution. The only contestation is whether the statute's foreign target language is sufficient or requires additional Fourth Amendment warrant requirement independent of statute.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the strict reading preserves Fourth Amendment protections: U.S. person communications remain protected by warrant requirement, and incidental data is deleted rather than accessed discretionally. FBI is listed as payer because it bears the constraint of being barred from queries for domestic crimes—it loses access to 702 data for law enforcement purposes. U.S. persons abroad and constitutional rights holders are beneficiaries because the constraint protects their communications integrity through deletion and warrant requirement. Suppression is low (0.08) because the constraint operates through statutory rule and court oversight, not coercive enforcement against resisters—the rule is transparent and bound by law, not maintained through hidden coercion. Theater is low (0.12) because the constraint's functional purpose (balance foreign intelligence and constitutional protection) and its actual operation (foreign target limit, minimization, warrant requirement) are aligned—there is minimal performative overhead. The time-series measurements show stable extractiveness and theater over the interval: the constraint is not accumulating extraction or theatricality; it is operating as designed under the strict reading. This stability is consistent with a Rope constraint whose coordination function remains functional and non-extractive.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (foreign intelligence agencies) and the payer seat (FBI law enforcement) should diverge sharply under the strict reading: from the intelligence perspective, the foreign target limit and minimization are workable constraints that enable rapid collection without warrant; from the law enforcement perspective, the categorical bar on domestic queries is a substantial loss of access to incidental intelligence data. However, both seats operate within a common constitutional framework that privileges warrant requirement for U.S. person communications. The engine should compute the foreign intelligence seat's directionality as low-target (benefits from speed, constrained by foreign target limit but not critically), and the FBI seat's directionality as high-target (loses access, constrained from domestic queries). The constraint's classification should diverge by seat: rope or scaffold from intelligence perspective (coordination + temporary foreign-target focus), tangled-rope or snare from FBI perspective (coordination benefit barred, extracted loss of access). The strict reading creates this divergence because it privileges the constitutional protection over law enforcement access.
 *
 * DIRECTIONALITY LOGIC:
 *   The foreign intelligence agencies are near-beneficiary (d~0.25): they retain the foreign target authority and can conduct fast surveillance without individual warrants, but they bear the minimization burden. FBI is the target (d~0.75): it loses the ability to use 702 data for domestic crimes, a constraint imposed by statutory language and court oversight. U.S. persons abroad and constitutional rights holders benefit from the warrant requirement and deletion minimization—they are near-beneficiary (d~0.15). The directionality divergence is driven by the structural fact that the constraint redistributes access: intelligence gains speed, law enforcement loses breadth, constitutional rights holders gain protection. No directionality override is needed; the structural data (foreign target beneficiary + FBI victim + warrant requirement) produces the right d values through standard derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (balance foreign intelligence speed with constitutional protection) remains live: the statutory foreign target language addresses the problem by enabling 702 collection while preserving Fourth Amendment warrant requirement for domestic use. Under the strict reading, mandatrophy has NOT resolved: the founding problem persists, the constraint remains functional, and the claim (rope) and metrics (low extraction, low theater) are consistent. However, the claim/metric independence rule applies: the ENGINE should test whether the metrics truly support rope classification or whether measured extraction is higher than 0.15. If measured extraction drifts upward (suggesting incidental_collection_reading practice: FBI queries of 702 for domestic crimes, warrantless use of incidental U.S. person data), the constraint would reclassify as tangled-rope or snare despite the strict reading's claim. This divergence—claim vs. metrics—is the measurement the corpus takes. The constraint is authored under strict reading premises (low extraction because of warrant requirement and deletion minimization), but the empirical question is whether actual practice honors those premises. Omega variables document this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_as_deletion_vs_access_restriction,
    'Does the statutory minimization requirement mandate deletion of incidental U.S. person data, or does it permit retention with restricted access?',
    'FISA court opinions interpreting minimization procedures; congressional oversight reports evaluating actual deletion vs. retention of incidental U.S. person communications; inspector general audits of minimization compliance.',
    'If deletion: the strict reading holds, extractiveness remains low (0.15), U.S. persons retain data integrity. If access-restricted retention: extractiveness rises substantially (0.45+), the constraint reclassifies to tangled-rope or snare, incidental collection reading becomes empirically operative. This is the axis of greatest structural divergence between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimization_as_deletion_vs_access_restriction, empirical, 'Whether minimization means deletion or discretionary access restriction for incidental U.S. person data.').

omega_variable(
    fbi_query_compliance_with_domestic_bar,
    'Does FBI actually refrain from querying the 702 database for domestic criminal investigations, or does it query using foreign intelligence framing when domestic cases have foreign nexus?',
    'FISA court review of query logs and use purposes; whistleblower testimony regarding actual FBI query practices; congressional oversight findings on domestic query rates.',
    'If FBI refrains: the strict reading''s categorical bar holds, low extraction persists. If FBI queries under foreign intelligence cover: the constraint operates as tangled-rope (coordination benefit of speed + extraction via query access), extraction rises to 0.35+, the incidental reading becomes operative in practice despite statutory language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fbi_query_compliance_with_domestic_bar, empirical, 'Whether the statutory bar on domestic queries is honored in actual FBI practice.').

omega_variable(
    statutory_sufficiency_of_fourth_amendment_protection,
    'Is the statutory foreign target requirement and minimization mandate sufficient constitutional protection for U.S. persons, or does Fourth Amendment independently require warrant for any search of U.S. person communications?',
    'Supreme Court decision interpreting Fourth Amendment applicability to 702 collection; constitutional scholarship consensus on warrant requirement scope; executive branch legal opinions on Fourth Amendment baseline.',
    'If statutory is sufficient: strict reading and rope classification hold; Fourth Amendment protections are adequately preserved through warrant requirement for domestic use. If Fourth Amendment independently requires warrant: constitutional-floor reading becomes operative, the constraint gains an external constitutional floor, extractiveness drops to ~0.08 (constitutional requirement overrides statutory interpretation), and the constraint reclassifies as mountain-adjacent (natural law floor preventing any warrantless search).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_sufficiency_of_fourth_amendment_protection, conceptual, 'Whether Fourth Amendment independently constrains 702 collection beyond statutory foreign target language.').

omega_variable(
    committer_reading_identity,
    'Is this constraint one reading of the FISA 702 kernel, or is it the kernel''s correct structural interpretation?',
    'The constraint is authored as one reading among three sibling readings that contest the same statutory text. This is not a meta-question about the constraint itself, but about the kernel framing: the constraint''s truth-status depends on whether the FISA 702 text admits multiple readings (kernel framing true) or admits only one correct interpretation (constraint is the interpretation, no siblings).',
    'Kernel framing is correct (adopted in this story): the reading_relations and axioms fields distinguish this reading''s premises from siblings'', and omegas 1–3 document empirical and conceptual resolutions that would shift between readings. If non-kernel framing (this is the statute''s true meaning, siblings are errors): the cs_structure block should be empty, reading_relations and axioms should not appear, and the constraint should claim mountain-like necessity (the statute says what it says). The reading framing is structural: it changes where disagreement is located (in interpretation vs. in truth-value) and what counts as evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_identity, conceptual, 'Whether the FISA 702 statutory text admits contested readings (kernel framing) or one determinate interpretation (non-kernel framing).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(fisa_tr_t5, observed).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(fisa_tr_t10, observed).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement_basis(fisa_tr_t15, observed).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(fisa_tr_t20, observed).
narrative_ontology:measurement(fisa_tr_t25, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(fisa_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(fisa_be_t5, observed).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement_basis(fisa_be_t10, observed).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(fisa_be_t15, observed).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(fisa_be_t20, observed).
narrative_ontology:measurement(fisa_be_t25, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(fisa_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.07).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement_basis(fisa_su_t5, observed).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement_basis(fisa_su_t10, observed).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement_basis(fisa_su_t15, observed).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(fisa_su_t20, observed).
narrative_ontology:measurement(fisa_su_t25, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(fisa_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__foreign_target_strict_reading, 0.08).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The FISA 702 statutory text kernel decomposes into three constraint stories, each a reading that instantiates different ε values and beneficiary/victim structures from the same statutory language. The foreign_target_strict_reading (this constraint) interprets the statute as sufficient protection; it treats U.S. persons as beneficiaries (warrant requirement preserved, incidental data deleted), computes low extraction (0.15), and claims rope type. The incidental_collection_reading interprets the statute as permitting retention and query of incidental data for foreign intelligence purposes; it may list U.S. persons as victims (warrantless access risk), computes medium-high extraction (0.45+), and claims tangled-rope or snare type. The constitutional_floor_reading treats Fourth Amendment as independent override of statutory interpretation; it adds a constitutional constraint on top of statutory language, computes very low extraction (~0.08), and claims rope or mountain-adjacent type. These three stories are not alternative measurements of the same constraint—they are different constraints instantiated by different readings of the same kernel. They are linked by affects_constraints to model the constraint family relationship. No reading answers the other readings or supersedes them via network logic—they remain live positions held by different institutional and constitutional actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__foreign_target_strict_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
