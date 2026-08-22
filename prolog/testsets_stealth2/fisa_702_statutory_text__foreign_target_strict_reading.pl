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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: Section 702 Foreign-Target Strict Reading (Deletion-Grade Minimization Boundary)
 *   domain: legal/constitutional/national_security
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the section
 *   702 statutory text read strictly, such that targeting reaches only
 *   communications whose sender and primary investigative interest are both
 *   non-U.S. persons located abroad, and incidentally acquired U.S.-person
 *   content undergoes deletion-grade minimization that leaves it inaccessible
 *   for domestic purposes. KEY AGENTS (by structural relationship): -
 *   non_us_persons_in_target_aperture: residual target (powerless/trapped) -
 *   collected warrantlessly inside the narrowed aperture with no remedy; -
 *   us_persons_incidentally_collected: protected class (moderate/constrained)
 *   - incidental capture purged, not queryable domestically; -
 *   nsa_sigint_operators: regulated operator and receipt seat
 *   (institutional/constrained) - bears compliance costs, receives the
 *   collection product; - fbi_domestic_investigators: forgone-capability
 *   bearer (institutional/constrained) - barred from domestic-purpose
 *   queries; - fisc_minimization_reviewers and
 *   congressional_intelligence_committees: administering and authoring seats
 *   (institutional/analytical); - civil_liberties_litigators: mission
 *   beneficiary (organized/mobile); - pclob_oversight_analysts: analytical
 *   observer (institutional/analytical); - foreign_allied_governments:
 *   excluded voice (powerful/constrained). The epsilon referent is the
 *   standing 702 arrangement as this reading assesses it - low, because
 *   rights-holders retain protections - not the arrangement the sibling
 *   incidental reading licenses. The claim and the metrics are independently
 *   authored facts: claimed_type records the structure I believe true (a
 *   coordination boundary with asymmetric residual extraction, actively
 *   enforced), while the metric values record what I believe descriptively
 *   accurate of the reading's operation; the engine computes per-seat types
 *   and any divergence from the claim is the datum.
 *
 * KEY AGENTS:
 *   - non_us_persons_in_target_aperture: residual target (powerless/trapped) - collected warrantlessly inside the narrowed aperture, no U.S.-law remedy
 *   - us_persons_incidentally_collected: protected class (moderate/constrained) - incidental capture deleted rather than gated, closed to domestic use
 *   - nsa_sigint_operators: dual-positioned operator (institutional/constrained) - pays compliance costs, receives collection product and program legitimacy
 *   - fbi_domestic_investigators: forgone-capability bearer (institutional/constrained) - categorical bar on domestic-crime queries
 *   - fisc_minimization_reviewers: enforcing interpreter (institutional/analytical) - conditions approvals, adjudicates non-compliance
 *   - congressional_intelligence_committees: author and leverage holder (institutional/analytical) - sunset-driven reauthorization
 *   - civil_liberties_litigators: mission beneficiary (organized/mobile) - litigates to hold the strict boundary
 *   - pclob_oversight_analysts: analytical observer (institutional/analytical) - independent audit of minimization reality
 *   - foreign_allied_governments: excluded voice (powerful/constrained) - nationals collected, no forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.16).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.55).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "Section 702 Foreign-Target Strict Reading (Deletion-Grade Minimization Boundary)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "legal/constitutional/national_security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:has_sunset_clause(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, 'be53e23c-3785-4d43-9eff-d8a5bbd619b9').
narrative_ontology:cs_kernel_codification('be53e23c-3785-4d43-9eff-d8a5bbd619b9', fixed_text).
narrative_ontology:cs_authority_grounding('be53e23c-3785-4d43-9eff-d8a5bbd619b9', lineage).
narrative_ontology:cs_interpretation_layer_present('be53e23c-3785-4d43-9eff-d8a5bbd619b9').
narrative_ontology:cs_reading_relation('be53e23c-3785-4d43-9eff-d8a5bbd619b9', fisa_702_statutory_text__incidental_collection_reading, coexists_with).
narrative_ontology:cs_reading_relation('be53e23c-3785-4d43-9eff-d8a5bbd619b9', fisa_702_statutory_text__constitutional_floor_reading, influences).
narrative_ontology:cs_axiom('be53e23c-3785-4d43-9eff-d8a5bbd619b9', foundational, statutory_foreign_boundary_marks_warrant_line).
narrative_ontology:cs_axiom_status(statutory_foreign_boundary_marks_warrant_line, holdable).
narrative_ontology:cs_axiom_grounding('be53e23c-3785-4d43-9eff-d8a5bbd619b9', statutory_foreign_boundary_marks_warrant_line, conventional).
narrative_ontology:cs_axiom('be53e23c-3785-4d43-9eff-d8a5bbd619b9', secondary, incidental_us_person_data_deleted_not_gated).
narrative_ontology:cs_axiom_status(incidental_us_person_data_deleted_not_gated, holdable).
narrative_ontology:cs_axiom_grounding('be53e23c-3785-4d43-9eff-d8a5bbd619b9', incidental_us_person_data_deleted_not_gated, conventional).
narrative_ontology:cs_reference_frame('be53e23c-3785-4d43-9eff-d8a5bbd619b9', foreign_intelligence_exclusivity_framework).
narrative_ontology:cs_drift_state('be53e23c-3785-4d43-9eff-d8a5bbd619b9', post_risaa_oversight_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('be53e23c-3785-4d43-9eff-d8a5bbd619b9', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_in_target_aperture).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, nsa_sigint_operators).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, nsa_sigint_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communicate internationally and can appear in section 702 acquisitions when a foreign target sits on the other end of a call or email or is discussed in traffic. Under this reading their incidentally swept content is subject to minimization procedures that purge it from the repository rather than merely fence it off, and it cannot be reached for domestic investigative purposes without leaving the 702 framework entirely. Their practical protection depends on deletion working at scale, and they hold no individual veto over initial acquisition.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected, beneficiary,
    moderate, biographical, constrained, national).

% Run both the collection and the compliance machinery: build selector sets confined to non-U.S. senders with non-U.S. investigative interest, operate deletion-grade minimization pipelines, absorb audit and documentation overhead, and surrender query flexibility. The same arrangement delivers the foreign-intelligence product they exist to produce and supplies a legally defensible foundation that shields the program's political viability; they lose operational latitude and gain a durable authorization.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, nsa_sigint_operators, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, nsa_sigint_operators, beneficiary).

% Work criminal and counterintelligence caseloads that sometimes brush against foreign-intelligence holdings. This reading categorically closes the repository to them for domestic-crime purposes: no backdoor queries, and any U.S.-person-touching search requires individualized process obtained outside 702. They carry slower casework and a hard evidentiary wall while retaining audited access for bona fide foreign-intelligence purposes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_investigators, payer,
    institutional, biographical, constrained, national).

% Are the people the aperture is built around: non-U.S. senders corresponding from abroad with subjects of foreign-intelligence interest. Their communications enter United States custody on certification-level authorization rather than individualized warrants, without consent, notice, or any U.S.-law remedy. The strict reading narrows who qualifies as a proper target and requires genuine foreign purpose, but extends them no affirmative protection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_in_target_aperture, payer,
    powerless, biographical, trapped, global).

% Review certifications and targeting and minimization procedures on a recurring cycle, condition approvals, adjudicate government non-compliance, and issue redacted opinions. Their procedural rulings are the operative surface on which the strict boundary is enforced; the depth and skepticism of their docket determine how much of the statutory limit is real.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisc_minimization_reviewers, agenda_setter,
    institutional, generational, analytical, national).

% Author and periodically reauthorize the statute against a hard expiration date. Each reauthorization cycle re-litigates query rules, warrant thresholds, and the scope of minimization, giving them leverage that keeps the agencies negotiating. Their incentives mix oversight prerogative with deference to intelligence equities.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congressional_intelligence_committees, agenda_setter,
    institutional, generational, analytical, national).

% Litigate to pin the statute to its strictest available meaning, publish analyses of documented compliance incidents, and press for deletion-grade minimization and warrant requirements on queries. The persistence of the arrangement as a strict boundary is their professional deliverable, and they can shift between courts, oversight testimony, and the reauthorization arena.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, civil_liberties_litigators, beneficiary,
    organized, biographical, mobile, national).

% Audit the program end to end, quantify incidental U.S.-person collection, test whether minimization deletes content or merely gates access, and report findings publicly. They constitute the principal independent check on whether the strict statutory reading describes operational reality.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, pclob_oversight_analysts, observer,
    institutional, biographical, analytical, national).

% Watch their nationals collected under a regime they did not consent to and cannot litigate in. They negotiate data-sharing frameworks and lodge diplomatic objections, but no seat in the U.S. authorization process represents them. The arrangement allocates protection by citizenship, and they stand on the unprotected side of that line.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, foreign_allied_governments, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, nsa_sigint_operators).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single legally authorized foreign-signals-intelligence capability while drawing an enforceable boundary that keeps the resulting repository closed to domestic law-enforcement use. Targeting criteria, minimization procedures, and query rules solve, once and centrally, the collective-action problem of operating mass-capacity collection without letting the same apparatus become a domestic policing instrument.
% TRANSFER_FUNCTION: Moves communicative content from non-U.S. persons abroad into government custody with full exposure; moves compliance labor and forgone query capability from the intelligence community; and moves privacy protection and evidentiary separation to U.S. persons, whose incidentally acquired communications are deleted rather than made available for domestic purposes.
% ABSENT_VOICES: The collected non-U.S. persons have no seat: no foreign national participates in targeting, and security-clearance amicus participation before the FISA court covers procedure rather than party standing. Allied governments whose nationals are swept in object diplomatically but hold no forum. Criminal defendants lack a guaranteed notice channel when 702-derived information touches their cases. Unanimity around the strict boundary arises among parties who all hold U.S. legal standing; the people bearing the residual extraction were never in the room.
% DISAPPEARANCE_RATIONALE: If the strict boundary vanished overnight, one of two rearrangements follows: the looser incidental-collection interpretation governs by default, opening the repository to domestic queries and weakening minimization, with U.S.-person exposure jumping sharply; or courts impose a categorical warrant floor instead, pausing large parts of collection pending individualized process. Either way the program's scope, the FBI's case workflows, and the oversight calendar reorganize. The boundary is load-bearing for how all three branches allocate surveillance authority.
% FOUNDING_PROBLEM: After the Church Committee exposed warrantless domestic surveillance programs and September 11 exposed the difficulty of acquiring foreign-to-foreign traffic transiting U.S. infrastructure, Congress built section 702 to collect the communications of non-U.S. persons reasonably believed to be abroad without individually naming every target, while statutorily walling Americans off from the resulting apparatus.
% FOUNDING_PROBLEM_CORROBORATION: The foreign-collection problem is attested live by sources outside the beneficiary set: the Privacy and Civil Liberties Oversight Board's program report documents continuing reliance on 702 for terrorism, cyber, and counterintelligence production; annual transparency filings show tens of thousands of targets; and allied states run parallel statutes for the same underlying problem. Intelligence-community attestations of necessity are interested and discounted; independent corroboration rests with the oversight board's findings and comparative foreign practice. Whether the protective half of the founding design remains live is precisely the dispute among the three readings of the kernel.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.16) because the reading's core operation protects rights-holders: U.S.-person content is deleted rather than retained-and-queryable, and the aperture itself is narrowed by requiring both a non-U.S. sender and a genuine non-U.S. investigative interest. The residual extraction is real but bounded - the warrantless exposure of non-U.S. persons inside the aperture (constant across all readings) plus deletion that is imperfect at scale in documented compliance incidents. Suppression (0.55) is authored as a raw structural property and is NOT scaled by power or scope in the engine's arithmetic - only extractiveness is scaled. It reflects criminal liability for unauthorized electronic surveillance, court-conditioned procedures, and audit mandates, tempered by the existence of lawful advocacy channels: agencies resist through lobbying and reauthorization politics rather than defiance. Theater ratio (0.25) reflects functioning deletion infrastructure and genuine audits alongside ceremonial elements (certification boilerplate, oversight reporting that lags operations by years). Accessibility collapse (0.35) is low because well-understood substitutes persist: collection under Executive Order 12333 outside the FISA framework entirely, liaison relationships, and compelled-provider mechanisms abroad - understanding the strict 702 boundary does not close the government's alternatives. Resistance (0.55) records sustained institutional pushback from the governed agencies at each reauthorization, with partial successes in loosening rounds. The temporal series shares one grid (t=0,3,6,9,12,15,18) so every metric is authored at every examined point; extraction declines as minimization hardened (ending the upstream about-collection practice and tightening query rules), theater declines as audit infrastructure matured, and the suppression series RISES deliberately - the enforcement machinery maintaining the strict boundary intensified as agency pushback grew, culminating in warrant requirements on U.S.-person-touching queries. This is an enforcement-ratchet trajectory, not decay.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute differently despite nominally identical institutional standing. From the NSA operator seat the arrangement is a manageable overhead that purchases durability - the same structure that costs them query flexibility delivers the product and shields the program politically, which is why the stakeholder carries a genuine secondary beneficiary role. From the FBI investigator seat the identical text operates as categorical capability denial. From the non-U.S. target seat the arrangement's residue is total exposure: narrowed, perhaps, but warrantless and remediless. From the U.S.-person seat it is protection. Same-level differentiation runs through role and exit, not power: the agencies share institutional rank but differ in what the boundary costs and returns them, and the engine computes these divergences from the structural data rather than from the authored claim. Coalition potential among the powerless is noted but weak here: non-U.S. targets cannot aggregate inside any U.S. forum, which is itself part of why the residual extraction persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to seats near the beneficiary end of d: us_persons_incidentally_collected (protected content, constrained exit but net subsidy from the arrangement) and civil_liberties_litigators (mission-level benefit, mobile exit pushes them toward arbitrage-grade positioning on this specific constraint). Declared victims map toward the target end: non_us_persons_in_target_aperture (powerless and trapped, nearest full-target), fbi_domestic_investigators (institutional but constrained, bearing capability loss), and nsa_sigint_operators. The NSA seat is the deliberate complication: the automatic derivation from victim-declaration-plus-constrained-exit would place them near full target, overstating their experienced extraction, because they also receive the collection product (they are the gain_flow seat) and harvest legitimacy. A directionality_overrides entry could correct this, but overrides key on the power_atom, and five of the nine seats share the institutional atom with sharply different true directionalities - a single institutional override would corrupt the FISC, congressional, and PCLOB placements to fix one. I therefore omitted overrides and left the dual position expressed through the secondary_role and gain_flow surfaces, which the engine reads alongside the victim declaration. Scope amplification applies modestly at the program's global collection scope; the national-scale domestic-access boundary carries less amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - acquiring foreign-to-foreign communications efficiently without turning the apparatus on Americans - remains live, so mandatrophy is not resolved and the constraint is not a piton candidate. Classification discipline cuts both ways here. Reading the arrangement as a pure rope ignores the identifiable parties who pay through it: foreign nationals collected without consent or warrant, and the FBI's forfeited domestic query capability. Reading it as a snare ignores the genuine coordination function - the boundary is what makes mass-capacity foreign collection constitutionally and politically survivable at all, and U.S. persons are net beneficiaries with real, audited protections. Tangled_rope holds both halves. The structural feature that guards against future mandatrophy is the hard sunset clause: reauthorization forces periodic re-examination, which is why theater_ratio declines rather than climbs across the interval. If reauthorization degenerates into auto-extension ritual while the foreign-collection problem evolved away, theater would rise, founding_problem_status would flip toward dead, and piton drift would begin - the measurement series is positioned to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the foreign_target_strict_reading of kernel fisa_702_statutory_text; which reading is operative determines the entire victim set and classification. What happens structurally if a sibling reading is adopted instead?',
    'Authoritative adoption of one reading through FISC precedent, Article III litigation, or codification in a reauthorization act; cross-file comparison with fisa_702_statutory_text__incidental_collection_reading and fisa_702_statutory_text__constitutional_floor_reading.',
    'Adopting the incidental reading adds U.S. persons to the victim set and drives extraction sharply upward (retention plus warrantless domestic-purpose query); adopting the constitutional floor inserts a warrant gate that partially supersedes the statutory line and may push the arrangement toward a different type altogether. This file''s epsilon is valid only for its own reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: classification is indexed to one reading of a contested statutory kernel.').

omega_variable(
    eo_12333_displacement,
    'Does strict-702 minimization genuinely shrink government access to U.S.-person communications, or does collection displace to Executive Order 12333 channels and foreign liaison arrangements that sit entirely outside the FISA framework?',
    'Cross-authority accounting in PCLOB and Inspectors General reporting comparing total U.S.-person exposure across 702, EO 12333, and liaison-sourced holdings over the same period.',
    'Substantial displacement means the strict reading''s protective achievement is smaller than measured, accessibility_collapse is understated, and part of the apparent extraction decline is relocation rather than reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eo_12333_displacement, empirical, 'Whether the boundary protects or merely relocates collection.').

omega_variable(
    deletion_efficacy_at_scale,
    'Is deletion-grade minimization technically achievable at upstream collection volumes, or does purging incidental U.S.-person content function aspirationally while gated access does the real work?',
    'Technical audits of minimization pipelines against documented over-collection incidents and retention inventories; PCLOB-style testing of whether flagged content is destroyed or archived.',
    'If deletion is aspirational, theater_ratio is materially higher than authored, the incidental reading describes operational reality more closely than this reading, and extraction is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deletion_efficacy_at_scale, empirical, 'Whether minimization-as-deletion is real or performative.').

omega_variable(
    foreign_national_standing_in_epsilon,
    'Do non-U.S. persons abroad bear cognizable extraction for classification purposes, or does the indexical seat count only parties with U.S. constitutional standing - given that the framework''s beneficiary/victim declarations are what drive effective extraction?',
    'Conceptual settlement in the framework''s treatment of extraterritorial rights-holders: whether victim declarations covering foreign nationals feed the directionality derivation or fall outside the counted seat set.',
    'Excluding foreign nationals drops extraction toward the coordination-cost floor and strengthens a rope reading; including them sustains the tangled_rope reading with the foreign aperture as the extraction leg. This is the largest single lever on this story''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_national_standing_in_epsilon, conceptual, 'Whose interests register in epsilon for an extraterritorial collection regime.').

omega_variable(
    reauthorization_ritual_risk,
    'Do sunset-driven reauthorization cycles function as genuine congressional leverage that enforces the strict boundary, or are they converging on auto-extension ritual in which the sunset clause persists while scrutiny hollows out?',
    'Longitudinal coding of reauthorization cycles: amendment density, floor debate substance, committee report engagement with compliance findings, and vote margins across successive cycles.',
    'Ritual convergence would reverse the declining theater_ratio trajectory, decouple the sunset clause from its enforcement function, and begin the drift toward piton that the current measurements do not show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reauthorization_ritual_risk, empirical, 'Whether the sunset clause remains a live enforcement lever.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(f702_strict_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(f702_strict_tr_t3, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(f702_strict_tr_t6, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(f702_strict_tr_t9, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 9, 0.33).
narrative_ontology:measurement(f702_strict_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(f702_strict_tr_t15, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(f702_strict_tr_t18, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 18, 0.25).

% Extraction over time
narrative_ontology:measurement(f702_strict_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(f702_strict_be_t3, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 3, 0.29).
narrative_ontology:measurement(f702_strict_be_t6, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 6, 0.27).
narrative_ontology:measurement(f702_strict_be_t9, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 9, 0.23).
narrative_ontology:measurement(f702_strict_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(f702_strict_be_t15, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(f702_strict_be_t18, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 18, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(f702_strict_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(f702_strict_su_t3, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 3, 0.34).
narrative_ontology:measurement(f702_strict_su_t6, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(f702_strict_su_t9, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 9, 0.42).
narrative_ontology:measurement(f702_strict_su_t12, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(f702_strict_su_t15, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement(f702_strict_su_t18, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 18, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question of what section 702 permits regarding Americans is three structurally distinct claims, not one observable-dependent constraint. This file (foreign_target_strict_reading) authors epsilon for the standing arrangement as the strict reading assesses it - low, because rights-holders retain protections and incidental content is deleted. fisa_702_statutory_text__incidental_collection_reading authors epsilon for the same statutory text under the retention-and-query permission structure - substantially higher, because U.S. persons join the victim set. fisa_702_statutory_text__constitutional_floor_reading authors epsilon for the arrangement under a categorical warrant requirement - highest on the existing practice, since any warrantless query of U.S.-person content registers as violation. The strict reading sits upstream of both: its deletion-grade minimization is cited as evidence in floor-reading disputes (that the statutory line, honored, moots the warrant question), and its tightening pressures what the incidental reading can justify. All three files link each other through affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
