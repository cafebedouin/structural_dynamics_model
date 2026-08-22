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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: FISA 702 — Foreign Target Strict Reading
 *   domain: Constitutional Law / National Security / Surveillance Policy
 *
 * SUMMARY:
 *   This story instantiates the foreign-target strict reading of the FISA
 *   Section 702 statutory kernel: the statute's 'foreign target' language is
 *   read as a genuine, enforceable limit confining collection to non-U.S.
 *   persons abroad, with incidental U.S. person data subject to mandatory
 *   minimization understood as deletion, and FBI querying of that data for
 *   domestic criminal purposes categorically prohibited absent an
 *   individualized warrant. Under this reading, U.S. persons are removed from
 *   the victim set entirely — they retain full Fourth Amendment protection
 *   because the statute is read not to authorize any domestic-purpose access
 *   to their incidentally collected content. The remaining extraction in this
 *   reading falls on foreign targets abroad, who have no constitutional or
 *   statutory shield and whose communications are the intended, lawful object
 *   of the program. This is a distinct constraint from the
 *   incidental_collection_reading (which treats warrantless domestic queries
 *   of incidental U.S. person data as statutorily permitted) and from the
 *   constitutional_floor_reading (which holds any 702 query of U.S. person
 *   content is a Fourth Amendment search regardless of statutory text). Each
 *   reading has a different victim set and a different ε; they are linked as
 *   siblings of the same kernel, not measurement variants of one constraint.
 *
 * KEY AGENTS:
 *   - nsa_fbi_collection_agencies: administers targeting and minimization (institutional/arbitrage) — sets and enforces the bright line
 *   - foreign_targets_abroad: primary and only victim under this reading (powerless/trapped) — bears the extraction the statute is designed to permit
 *   - us_persons_incidentally_swept_and_protected_by_minimization: beneficiary of the deletion rule (moderate/constrained) — shielded rather than extracted from under this reading
 *   - fbi_domestic_investigators: excluded from database access for domestic crimes (institutional/trapped by the rule itself)
 *   - fisa_court: analytical oversight seat (institutional/analytical)
 *   - civil_liberties_advocates: excluded from the certification process, dispute the deletion premise in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.2).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA 702 — Foreign Target Strict Reading").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "Constitutional Law / National Security / Surveillance Policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '2c1c4d5a-1dd0-48dc-82c5-32c142c5d812').
narrative_ontology:cs_kernel_codification('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', formalized).
narrative_ontology:cs_authority_grounding('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', lineage).
narrative_ontology:cs_interpretation_layer_present('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812').
narrative_ontology:cs_reading_relation('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', foundational, minimization_requires_deletion_not_mere_access_restriction).
narrative_ontology:cs_axiom_status(minimization_requires_deletion_not_mere_access_restriction, holdable).
narrative_ontology:cs_axiom_grounding('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', minimization_requires_deletion_not_mere_access_restriction, conventional).
narrative_ontology:cs_axiom('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', foundational, statutory_foreign_target_language_is_independently_binding).
narrative_ontology:cs_axiom_status(statutory_foreign_target_language_is_independently_binding, holdable).
narrative_ontology:cs_axiom_grounding('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', statutory_foreign_target_language_is_independently_binding, conventional).
narrative_ontology:cs_reference_frame('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', foreign_intelligence_surveillance_act_1978_amendment_compromise).
narrative_ontology:cs_drift_state('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', post_2008_faa_reauthorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c1c4d5a-1dd0-48dc-82c5-32c142c5d812', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_targeting_non_us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_swept_and_protected_by_minimization).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_targets_abroad).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_extraterritoriality_limit).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_textualism_on_foreign_target_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers targeting and minimization procedures under this reading: certifies that targets are non-U.S. persons reasonably believed to be abroad, and is statutorily required to purge or wall off incidentally collected U.S. person content from domestic investigative use. Retains discretion over targeting decisions but is bound by the minimization-as-deletion rule this reading enforces.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, nsa_fbi_collection_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Non-U.S. persons located outside the United States whose communications are the intended object of collection. They hold no Fourth Amendment protection under this reading and no statutory minimization right; the entire extractive weight of the program is, by this reading's own design, meant to fall on them rather than on U.S. persons.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_targets_abroad, payer,
    powerless, biographical, trapped, global).

% U.S. persons whose communications are incidentally captured when communicating with a foreign target. Under this reading, their content must be minimized (deleted or purged) and is categorically inaccessible for domestic law enforcement queries absent an individualized warrant. They benefit from a bright-line exclusion rule rather than a case-by-case access-control regime.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_swept_and_protected_by_minimization, beneficiary,
    moderate, biographical, constrained, national).

% Domestic criminal investigators who, under this reading, are categorically barred from querying the 702 database for U.S. person identifiers in connection with domestic crimes. They would prefer database access for investigative leads but their position is foreclosed by this reading's minimization-as-deletion rule, not merely discouraged.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigators, excluded,
    institutional, immediate, trapped, national).

% Reviews and approves annual certifications and minimization procedures. Under this reading, its oversight role is to verify that the statutory foreign-target line and deletion requirements are actually implemented, rather than to adjudicate individualized warrant applications for incidental collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisa_court, observer,
    institutional, generational, analytical, national).

% Argue that even this strict reading understates residual risk because minimization procedures in practice retain more incidental U.S. person data than the deletion rule contemplates; they are not part of the certification process and their objections surface only through litigation or oversight hearings, not the statutory mechanism itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, nsa_fbi_collection_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates foreign intelligence collection against non-U.S. persons abroad by drawing a bright statutory line: only communications where both the sender and the primary investigative interest are non-U.S. persons located outside the U.S. fall within the targeting authority, with anything touching a U.S. person treated as incidental and subject to mandatory minimization.
% TRANSFER_FUNCTION: Moves surveillance burden onto non-U.S. persons abroad who have no constitutional or statutory shield under this reading, while U.S. persons who are incidentally swept up are (by this reading's design) shielded by deletion requirements rather than becoming an accessible investigative resource.
% ABSENT_VOICES: Foreign targets abroad have no voice in the U.S. legal or political process that authors or reviews this statute — they are the population the reading is structurally built to permit extraction from, and no domestic forum represents their interest. Civil liberties advocates who doubt minimization is implemented as deletion in practice are also structurally outside the certification loop.
% DISAPPEARANCE_RATIONALE: If this strict reading's bright line and deletion-based minimization rule vanished, the practical effect (absent the sibling readings taking its place) would be to remove the only structural firewall currently separating foreign intelligence collection from ordinary domestic law enforcement access to incidentally collected U.S. person communications; FBI query practices and minimization procedures would need to be renegotiated or replaced by warrant requirements or laxer access rules.
% FOUNDING_PROBLEM: Congress sought a statutory mechanism permitting the intelligence community to collect foreign intelligence from non-U.S. persons abroad using U.S. communications infrastructure, while responding to post-2008 controversy over warrantless surveillance by cabining incidental collection of U.S. persons and prohibiting its repurposing for domestic investigations.
% FOUNDING_PROBLEM_CORROBORATION: Congressional sponsors and DOJ/ODNI officials attest the foreign-target line and minimization-as-deletion regime remains faithful to and adequate for the founding compromise. Independent oversight bodies (PCLOB reports) and civil liberties organizations outside the intelligence community dispute that minimization functions as deletion in practice, citing documented backdoor query volumes — corroboration for 'live' status comes primarily from the administering agencies themselves, which this reading's own framework treats as sufficient but which sits inside, not outside, the benefiting institutional set.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.15) because this reading, by construction, excludes U.S. persons from the extraction it measures — the deletion-based minimization rule and the categorical query prohibition are read as functioning as designed, leaving the extractive core confined to foreign targets abroad who are outside the reading's own rights-bearing population. Suppression is modest (0.2) because the reading imposes real procedural constraints (deletion requirements, query prohibitions) rather than pure permissive latitude. Theater ratio is low (0.1) because under this reading the minimization procedures are not treated as performative — they are read as doing genuine deletion work, not mere paperwork. All three metrics rise only gradually across the interval, reflecting slow accretion of query volume and backdoor-search practice reported by oversight bodies even under a reading that holds the statutory line is being honored.
 *
 * DIRECTIONALITY LOGIC:
 *   Foreign targets abroad sit at the full-target end of directionality: trapped, powerless, no exit, no rights recognized by this reading. U.S. persons incidentally captured are treated, under this reading, as beneficiaries of the deletion rule rather than as victims — the statute's asymmetric extraction is directed outward, not at them, which is precisely the structural delta this reading claims relative to its siblings. The administering agencies sit near arbitrage exit: they set and can adjust targeting procedures within the statutory line. FBI domestic investigators are excluded rather than benefiting or paying directly — the rule forecloses an option they would otherwise want.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (permitting foreign intelligence collection while cabining domestic law-enforcement repurposing of incidental data) is read by this reading's proponents as still live and still being served by the statutory text as written. The mismatch signal here is the divergence between founding_problem_status='contested' and the reading's own claim that minimization functions as deletion: oversight reporting on backdoor query volumes (sourced from outside the certifying agencies) suggests the deletion premise this reading depends on may not hold in operational practice, which is exactly the kind of status-vs-verdict tension the R5 corroboration field is designed to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimization_as_deletion_in_practice,
    'Does minimization actually function as deletion of incidentally collected U.S. person data in operational practice, as this reading assumes, or does substantial incidentally collected content persist in queryable form?',
    'PCLOB and Inspector General audits of actual purge rates and backdoor query volumes against incidentally collected U.S. person identifiers, compared to the deletion timelines specified in minimization procedures.',
    'If deletion is substantially incomplete or query prohibitions are routinely waived under ''foreign intelligence purpose'' exceptions, this reading''s low ε for U.S. persons is empirically false and the constraint''s actual operation converges toward the incidental_collection_reading regardless of the statutory text this reading relies on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_as_deletion_in_practice, empirical, 'Whether deletion-based minimization is implemented as designed or is substantially theatrical.').

omega_variable(
    textualism_vs_constitutional_floor_priority,
    'Does the statutory foreign-target line this reading relies on do independent constraining work, or is it superseded in practice by whatever the Fourth Amendment independently requires (the constitutional_floor_reading)?',
    'Appellate or Supreme Court resolution of whether 702 backdoor queries constitute Fourth Amendment searches, which would settle whether the statutory line in this reading is doing load-bearing work or is redundant with (or overridden by) constitutional doctrine.',
    'If courts adopt the constitutional_floor_reading as controlling, this reading''s statutory textualism becomes a floor beneath a floor — legally accurate but practically non-binding, shifting real protective work to constitutional doctrine rather than the statute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualism_vs_constitutional_floor_priority, conceptual, 'Whether this reading''s statutory limit is independently operative or subordinate to constitutional doctrine.').

omega_variable(
    foreign_target_definitional_drift,
    'How rigorously is ''primary investigative interest'' in non-U.S. persons actually verified at the targeting stage, versus functioning as a certification formality that permits de facto interest in U.S.-person-linked communications?',
    'Review of targeting decision records and FISA Court minimization procedure approvals for cases where a nominally foreign target''s communications were selected substantially because of anticipated U.S. person participants.',
    'If targeting decisions are frequently driven by anticipated U.S. person content, the foreign-target line this reading depends on is narrower in practice than in text, and the reading''s low ε assumption weakens even for the reading''s own foreign-target population framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_target_definitional_drift, empirical, 'Whether the foreign-target certification requirement constrains targeting in practice or is a formal gate only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 4, 0.07).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 16, 0.14).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 4, 0.18).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 12, 0.19).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'FISA 702 foreign intelligence surveillance constraint' per the ε-invariance principle. foreign_target_strict_reading (this file, ε≈0.15, victim set = foreign targets abroad only) is linked to incidental_collection_reading (permits warrantless domestic-purpose queries of incidental U.S. person data, much higher ε for U.S. persons) and constitutional_floor_reading (holds Fourth Amendment analysis controls regardless of statutory text, effectively zeroing ε for U.S. persons under any statutory reading by requiring an independent warrant). Each reading is authored as a distinct constraint with its own stakeholders and metrics; none is a measurement variant of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
