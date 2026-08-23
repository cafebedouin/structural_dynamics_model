% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: FISA §702 Foreign-Target Limitation (Strict Statutory Reading)
 *   domain: constitutional law / national security / surveillance policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested FISA §702 statutory
 *   text: the strict reading, under which the foreign-target language
 *   categorically excludes U.S. persons from the permissible target set,
 *   requires minimization of incidentally collected U.S. person content as
 *   deletion rather than access restriction, and bars FBI queries of the 702
 *   database for domestic criminal purposes. The ε referent is the standing
 *   702 arrangement assessed by this reading's own lights: rights-holders
 *   retain Fourth Amendment protections, so base extractiveness from them is
 *   low (0.15). The sibling readings are different constraints, not
 *   alternative measurements of this one: the incidental_collection_reading
 *   authors high ε with U.S. persons in the victim set (retention and
 *   warrantless querying of incidentally collected content), and the
 *   constitutional_floor_reading bypasses statutory construction entirely,
 *   grounding a warrant requirement in the Fourth Amendment itself. Claimed
 *   type and metrics are authored independently within this file; the engine
 *   computes per-seat classifications from the structural data. KEY AGENTS
 *   (by structural relationship): - us_persons_rights_holders: primary
 *   beneficiary (moderate/constrained) — protected by the categorical line;
 *   no individual notice or standing - intelligence_community_agencies: payer
 *   with beneficiary secondary position (institutional/constrained) — bears
 *   compliance costs; collects the program's constitutional defensibility -
 *   foreign_intelligence_targets: payer (powerless/trapped) — the defined
 *   target set; no forum, no exit - foreign_intelligence_surveillance_court:
 *   agenda_setter (institutional/constrained) — administers targeting and
 *   minimization approval ex parte - congressional_overseers: agenda_setter
 *   (institutional/mobile) — wrote the line; can replace it at
 *   reauthorization - privacy_civil_liberties_oversight_board: observer
 *   (moderate/analytical) — the main non-ex parte compliance record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.42).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA §702 Foreign-Target Limitation (Strict Statutory Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional law / national security / surveillance policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'cb80108e-508f-429b-85b4-65942a994e34').
narrative_ontology:cs_kernel_codification('cb80108e-508f-429b-85b4-65942a994e34', fixed_text).
narrative_ontology:cs_authority_grounding('cb80108e-508f-429b-85b4-65942a994e34', distributed).
narrative_ontology:cs_reading_relation('cb80108e-508f-429b-85b4-65942a994e34', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb80108e-508f-429b-85b4-65942a994e34', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('cb80108e-508f-429b-85b4-65942a994e34', foundational, categorical_foreign_target_limitation).
narrative_ontology:cs_axiom_status(categorical_foreign_target_limitation, holdable).
narrative_ontology:cs_axiom_grounding('cb80108e-508f-429b-85b4-65942a994e34', categorical_foreign_target_limitation, conventional).
narrative_ontology:cs_axiom('cb80108e-508f-429b-85b4-65942a994e34', secondary, incidental_us_person_data_deletion).
narrative_ontology:cs_axiom_status(incidental_us_person_data_deletion, holdable).
narrative_ontology:cs_axiom_grounding('cb80108e-508f-429b-85b4-65942a994e34', incidental_us_person_data_deletion, conventional).
narrative_ontology:cs_axiom('cb80108e-508f-429b-85b4-65942a994e34', secondary, domestic_purpose_query_prohibition).
narrative_ontology:cs_axiom_status(domestic_purpose_query_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('cb80108e-508f-429b-85b4-65942a994e34', domestic_purpose_query_prohibition, conventional).
narrative_ontology:cs_reference_frame('cb80108e-508f-429b-85b4-65942a994e34', categorical_foreign_target_line).
narrative_ontology:cs_drift_state('cb80108e-508f-429b-85b4-65942a994e34', post_disclosure_oversight_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb80108e-508f-429b-85b4-65942a994e34', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, fourth_amendment_domestic_warrant_line).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, constitutional_avoidance_canon).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose communications are incidentally swept in when they contact overseas parties. Under this reading they sit outside the permissible target set: their incidentally collected content must be purged rather than retained for access, and it is unavailable for domestic criminal or regulatory use. Their protection is mediated entirely by institutions — statute, FISC minimization orders, oversight boards — and they receive no individual notice of incidental collection. Exit looks like encryption or withdrawal from international communication, which is impractical for most.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_rights_holders, beneficiary,
    moderate, biographical, constrained, national).

% NSA and FBI operate the collection and bear the compliance burden: building minimization procedures that purge incidentally collected U.S. person content, forgoing warrantless queries of the 702 database for domestic criminal purposes, and documenting compliance to the FISC and DOJ. In exchange, the foreign-target line is what makes the warrantless foreign collection program constitutionally defensible — without it the program would face a general-warrant challenge. They cannot exit the statute, and their institutional preference for the broader incidental reading is channeled into reauthorization lobbying rather than open noncompliance.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_agencies, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_community_agencies, beneficiary).

% Non-U.S. persons abroad whose communications are the authorized collection subject. The statute fixes them as the permissible target set; they have no Fourth Amendment standing, no U.S. forum, and no practical exit from communications infrastructure that transits U.S. providers. Under this reading their position is unchanged from any other reading of the text — the constraint's distinctive operation falls on the domestic side, not theirs.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_targets, payer,
    powerless, biographical, trapped, global).

% Approves annual targeting and minimization procedures and adjudicates compliance disputes; under this reading it would enforce minimization as deletion and reject query regimes that repurpose incidentally collected U.S. person content for domestic purposes. It is bound to its statutory role, sits ex parte, and its docket is the arrangement's primary enforcement surface.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_intelligence_surveillance_court, agenda_setter,
    institutional, generational, constrained, national).

% The intelligence and judiciary committees wrote the foreign-target language and reauthorize it on fixed cycles; they can tighten or loosen the line at each reauthorization and have repeatedly declined to adopt the broader incidental reading for U.S. person queries. Their exit is legislative rather than practical — they remain the seat that could replace this arrangement by amendment.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congressional_overseers, agenda_setter,
    institutional, generational, mobile, national).

% Audits minimization and query compliance and publishes reports; it attests from outside the operating agencies whether the strict line is honored in practice. Its findings are the main non-ex parte record of how much incidentally collected U.S. person content survives and for how long.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, privacy_civil_liberties_oversight_board, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a bright, judicially administrable line between foreign intelligence collection and domestic surveillance: the agencies get a single warrantless collection authority bounded to non-U.S. persons abroad, the FISC gets an administrable minimization standard, and the domestic population gets a categorical guarantee that its communications are not the target and that incidentally swept content is deleted rather than retained for use.
% TRANSFER_FUNCTION: Moves collection capability toward non-U.S. persons abroad and away from U.S. persons; moves compliance costs — minimization, deletion, query restrictions, documentation — onto the operating agencies; and moves legal protection to U.S. persons, whose incidentally collected content is rendered inaccessible for domestic purposes.
% ABSENT_VOICES: Foreign intelligence targets would object to the entire arrangement but have no U.S. forum: no Fourth Amendment standing, no notice, and an ex parte FISC. U.S. persons receive no individual notice of incidental collection and lack standing to challenge minimization failures; their interests appear only through amicus appointments, oversight boards, and congressional staff — never as parties.
% DISAPPEARANCE_RATIONALE: If the strict foreign-target line vanished overnight, the warrantless program would lose its constitutional defense: either the program would have to be rebuilt around individualized warrants — a massive operational restructuring — or a general-warrant challenge would force the same result. Query practice, minimization procedure, and the FISC's docket would all reorganize around the warrant requirement.
% FOUNDING_PROBLEM: After the Church Committee documented decades of warrantless surveillance of Americans, Congress built FISA to require individualized warrants for domestic surveillance; the 2008 amendments added a streamlined way to target non-U.S. persons abroad without individualized orders — and the foreign-target language is the line that keeps that efficiency from swallowing the domestic warrant requirement.
% FOUNDING_PROBLEM_CORROBORATION: Church Committee history and the FISA statute's own findings attest the founding problem from outside the benefiting parties; FISC opinions and Privacy and Civil Liberties Oversight Board reports corroborate that the foreign/domestic line-drawing problem remains live; even executive-branch testimony defending the program rests on the premise that U.S. persons are not the target — no party with a seat asserts the founding problem is dead.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint's distinctive operation is protective: what it adds to the underlying program is the domestic line, and under this reading the line holds — incidental content is deleted and domestic queries are barred. Suppression (0.42) is authored as a raw, unscaled structural property: it is real but directed at the operating agencies (mandatory deletion, query prohibition, FISC documentation), while alternatives — individualized warrants, programmatic restructuring at reauthorization — remain open, so it lacks the closed-exit character of a snare; the engine, not this file, scales extractiveness by directionality and scope. Theater (0.22) reflects a functional core — deletion and query restriction are substantive — with a growing ritual share as annual certification and compliance reporting have thickened. Accessibility collapse is low (0.35) because the alternative readings are not collapsed: the incidental reading remains the executive's operative position and a live litigable claim. Resistance (0.45) records the agencies' sustained institutional push for query authority at each reauthorization. All three series share one time grid (t=0..16, mapping 2008 enactment to the 2024 reauthorization, with t=5 at the 2013 disclosures and t=10 at the 2018 reauthorization); the suppression_requirement series is authored because this story specifically tracks enforcement machinery — the FISC's post-disclosure minimization scrutiny and the statutory query restrictions that accumulated after 2013 — not because suppression varies incidentally. Receipt surface: gain_flow is authored 'diffuse' as an affirmative checked claim — the constraint's product (domestic protection) accrues across the rights-holder population and no named seat captures it; the agencies accrue the underlying program's continuation, which is the kernel-level settlement's product rather than this constraint's own yield. fixing_cost is 'prohibitive': removing the strict line has been attempted at every reauthorization cycle and the warrant-requirement core for U.S. person queries has not yielded — the political and constitutional cost to the fixing seat exceeds the operational benefit the agencies would gain. Note that the diffuse-plus-prohibitive combination sits in the cell the prototype labels piton-flavored; here it reflects a valued, actively enforced protection (low theater, live function), not inertial maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the rights-holder seat the arrangement is protection: low extraction, genuine coordination. From the agency seat it is a binding operational limit whose legitimizing benefit the agencies also collect — a dual position the single-role derivation can only partially see. From the foreign-target seat the same arrangement is extraction with no exit and no voice: the constraint defines them as the permissible target set. The FISC seat sees an administrable standard; the congressional seat sees an adjustable line. The engine computes these divergences from the structural data; the rope claim is the domestic-settlement view, not an adjudication of the foreign-target seat.
 *
 * DIRECTIONALITY LOGIC:
 *   us_persons_rights_holders are declared beneficiaries, driving their d toward the beneficiary end. The agencies bear compliance costs (payer) while collecting the authorization benefit (secondary beneficiary) — their derived d should land near symmetric; no override is used because the dual declaration carries the structure. foreign_intelligence_targets carry role payer but appear in no victim declaration: the strict reading's rights frame does not count them, so the canonical fallback for powerless agents would understate their position as the constraint's defined target set. A single directionality override (powerless → 0.90) corrects this — it is unambiguous in this story because only the foreign-target seat holds that power atom; their d sits near the full-target end because the arrangement's operation directs surveillance at them specifically and they have no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authorizing efficient foreign collection without recreating warrantless domestic surveillance — is live, so mandatrophy does not attach: the constraint's function has not outlived its justification, and the founding-problem status is corroborated from outside the benefiting parties. The classification work here is the reverse of the usual case: this story prevents a coordination structure from being misread as extraction cover. The strict reading's constraint genuinely protects the domestic population; its low ε is not a cover story but the reading's operative content. The residual extraction questions are routed to omegas rather than forced into the classification: whether residual incidental exposure is larger than the reading's lights can see, and whether the domestic settlement purchases itself with burdens on the voiceless — a kernel-level question that belongs to the family, not to this file.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the fisa_702_statutory_text kernel; what structurally changes if the incidental_collection_reading is adopted instead?',
    'Statutory amendment or a sustained FISC/courts shift to the incidental reading; the sibling story authors the delta (U.S. persons enter the victim set, minimization becomes access restriction rather than deletion, ε rises toward 0.7+ from the rights-holder seat).',
    'Adoption of the sibling converts this file''s protective arrangement into the sibling''s extractive one; the two files must never be merged — their ε values differ by design, and the classification of the whole turns on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the 702 kernel governs.').

omega_variable(
    incidental_volume_growth,
    'Does the growth in incidentally collected U.S. person communications (platform migration, global traffic transiting U.S. providers) make residual exposure substantial even under strict minimization-as-deletion?',
    'Declassified minimization reports and FISC compliance filings quantifying incidentally collected U.S. person content and retention windows.',
    'If residual exposure is substantial, the 0.15 ε understates the rights-holder burden even under this reading, and the low-extraction profile erodes without any change in the statutory line.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_volume_growth, empirical, 'Whether strict minimization keeps pace with incidental collection volume.').

omega_variable(
    foreign_target_exclusion_burden,
    'Does the domestic settlement purchase its coordination with burdens on non-U.S. persons who have no voice in it — and should those burdens count when evaluating this constraint?',
    'A values determination (whether extraterritorial persons enter the evaluation''s ledger) plus comparative analysis of target-set breadth across readings; no empirical observation resolves it from inside this reading''s lights, which is why the foreign-target seat carries a directionality override rather than a victim declaration.',
    'If foreign-target burdens count, the kernel-level arrangement is a hybrid coordination/extraction structure regardless of which reading governs, and this file''s rope claim holds only for the domestic seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_target_exclusion_burden, preference, 'Whether the voiceless target set counts in the settlement''s ledger.').

omega_variable(
    line_erosion_trajectory,
    'Will the strict line hold across reauthorization cycles, or erode by attrition into the incidental reading''s operational practice while the statutory text stays unchanged?',
    'Successive reauthorization statutes, FISC minimization approvals, and FBI query-audit series compared across cycles.',
    'Erosion would leave this constraint formally intact but functionally displaced — drift toward the sibling file''s world without formal amendment; the theater_ratio series is the early indicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(line_erosion_trajectory, empirical, 'Whether the strict line survives reauthorization attrition.').

omega_variable(
    constitutional_floor_backstop,
    'If the constitutional_floor_reading is adopted (warrants required for U.S. person queries regardless of statute), does this reading''s constraint become redundant backstop or the vehicle that moots the constitutional question?',
    'Appellate or en banc treatment of whether U.S. person database queries are Fourth Amendment searches; doctrinal analysis of the avoidance relationship between the readings.',
    'If the floor reading governs, this file''s constraint survives as statutory belt-and-suspenders with near-zero independent extractive content; if this reading governs first, the floor question is avoided and the sibling''s docket shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_floor_backstop, conceptual, 'Relationship between the statutory strict reading and the constitutional floor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement_basis(fisa_tr_t3, observed).
narrative_ontology:measurement(fisa_tr_t5, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(fisa_tr_t5, observed).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(fisa_tr_t8, observed).
narrative_ontology:measurement(fisa_tr_t10, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(fisa_tr_t10, observed).
narrative_ontology:measurement(fisa_tr_t13, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement_basis(fisa_tr_t13, observed).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(fisa_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 3, 0.11).
narrative_ontology:measurement_basis(fisa_be_t3, observed).
narrative_ontology:measurement(fisa_be_t5, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement_basis(fisa_be_t5, observed).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 8, 0.13).
narrative_ontology:measurement_basis(fisa_be_t8, observed).
narrative_ontology:measurement(fisa_be_t10, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(fisa_be_t10, observed).
narrative_ontology:measurement(fisa_be_t13, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 13, 0.15).
narrative_ontology:measurement_basis(fisa_be_t13, observed).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement_basis(fisa_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 3, 0.3).
narrative_ontology:measurement_basis(fisa_su_t3, observed).
narrative_ontology:measurement(fisa_su_t5, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement_basis(fisa_su_t5, observed).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement_basis(fisa_su_t8, observed).
narrative_ontology:measurement(fisa_su_t10, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(fisa_su_t10, observed).
narrative_ontology:measurement(fisa_su_t13, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 13, 0.41).
narrative_ontology:measurement_basis(fisa_su_t13, observed).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(fisa_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'FISA 702' covers at least three structurally distinct constraints. This file is the strict reading — a protective line with low ε and rights-holders benefited. The incidental_collection_reading instantiates the same text as a permission structure (high ε, U.S. persons in the victim set, warrantless query authority over retained incidental content). The constitutional_floor_reading removes the statutory question entirely, grounding a warrant requirement in the Fourth Amendment itself. Upstream/downstream: the constitutional floor pressures the statutory readings via constitutional avoidance; this reading, if adopted, shrinks the floor reading's docket (the influences edge). Each file carries its own ε; the family is linked through affects_constraints and must not be merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__foreign_target_strict_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
