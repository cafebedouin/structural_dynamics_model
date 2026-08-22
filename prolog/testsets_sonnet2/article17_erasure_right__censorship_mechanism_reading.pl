% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__censorship_mechanism_reading, []).

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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: Article 17 (GDPR Right to Erasure) as Weaponized Speech-Suppression Mechanism
 *   domain: Technology Governance / Data Protection Law / Competition Policy
 *
 * SUMMARY:
 *   This story instantiates the censorship-mechanism reading of the Article
 *   17 (GDPR right-to-erasure) kernel. Under this reading, the erasure
 *   right's public-interest and freedom-of-expression carve-outs are
 *   structurally underenforced relative to the compliance incentives
 *   platforms face, producing a de facto suppression channel that bad-faith
 *   requesters and reputation management intermediaries exploit against
 *   accurate reporting and public records. This is a distinct constraint from
 *   the privacy_fundamental_reading (which treats the same provision as
 *   protecting genuine data sovereignty against corporate retention) and the
 *   competitive_moat_reading (which treats it as an incumbent-protecting
 *   compliance-cost mechanism). The three readings share a text but diverge
 *   sharply in beneficiary/victim structure and in epsilon: this reading's
 *   referent is the standing erasure-adjudication arrangement as currently
 *   administered, assessed for its speech-suppression function specifically,
 *   not for its privacy-protective core, which is a separate reading with a
 *   separate low-epsilon story.
 *
 * KEY AGENTS:
 *   - bad_faith_erasure_requesters: primary beneficiary (moderate/mobile) — exploits low evidentiary bar to retract accurate adverse records
 *   - reputation_management_firms: organized beneficiary/agenda-shaper (organized/arbitrage) — commercializes bulk erasure filing
 *   - search_and_platform_operators: agenda_setter under asymmetric liability (institutional/constrained) — over-complies because wrongful retention is penalized, wrongful delisting is not
 *   - investigative_journalists and digital_archivists: primary victims (moderate-powerless/constrained-trapped) — accurate public-interest work delisted without adequate notice or appeal
 *   - data_protection_authorities: excluded structural referee (institutional/analytical) — reviews complaints reactively, rarely audits systemic overreach
 *   - ordinary_data_subjects: the mechanism's intended, genuine beneficiary population, structurally indistinguishable from bad-faith filers at intake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.71).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.78).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, snare).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 (GDPR Right to Erasure) as Weaponized Speech-Suppression Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "Technology Governance / Data Protection Law / Competition Policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, 'b3386015-4ef6-4d27-be5c-be5f9d626c87').
narrative_ontology:cs_kernel_codification('b3386015-4ef6-4d27-be5c-be5f9d626c87', formalized).
narrative_ontology:cs_authority_grounding('b3386015-4ef6-4d27-be5c-be5f9d626c87', extraction).
narrative_ontology:cs_interpretation_layer_present('b3386015-4ef6-4d27-be5c-be5f9d626c87').
narrative_ontology:cs_reading_relation('b3386015-4ef6-4d27-be5c-be5f9d626c87', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3386015-4ef6-4d27-be5c-be5f9d626c87', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('b3386015-4ef6-4d27-be5c-be5f9d626c87', foundational, accurate_public_record_presumptively_retained).
narrative_ontology:cs_axiom_status(accurate_public_record_presumptively_retained, holdable).
narrative_ontology:cs_axiom_grounding('b3386015-4ef6-4d27-be5c-be5f9d626c87', accurate_public_record_presumptively_retained, deontological).
narrative_ontology:cs_axiom('b3386015-4ef6-4d27-be5c-be5f9d626c87', foundational, erasure_functions_as_suppression_when_public_interest_underweighted).
narrative_ontology:cs_axiom_status(erasure_functions_as_suppression_when_public_interest_underweighted, holdable).
narrative_ontology:cs_axiom_grounding('b3386015-4ef6-4d27-be5c-be5f9d626c87', erasure_functions_as_suppression_when_public_interest_underweighted, empirically_contingent).
narrative_ontology:cs_reference_frame('b3386015-4ef6-4d27-be5c-be5f9d626c87', google_spain_narrow_relevance_balancing).
narrative_ontology:cs_drift_state('b3386015-4ef6-4d27-be5c-be5f9d626c87', post_transparency_report_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3386015-4ef6-4d27-be5c-be5f9d626c87', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, ordinary_data_subjects).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, individual_control_over_personal_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with adverse but factually accurate public records — fraud convictions, professional misconduct findings, documented public statements — file erasure requests framed as privacy claims. They exploit the low cost and low evidentiary bar of the request process, the asymmetric burden it places on publishers to justify retention, and platforms' incentive to comply rather than litigate. The mechanism lets them functionally retract published speech about themselves without ever engaging the truth of the record.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    moderate, biographical, mobile, national).

% Commercial firms that monetize erasure requests at scale on behalf of clients, filing bulk claims against search indexes and news archives. They have developed procedural expertise in maximizing compliance rates and shaping which claims platforms find easiest to grant, effectively administering a privatized takedown pipeline that platforms defer to rather than adjudicate.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, agenda_setter).

% Bear legal liability for non-compliance and face regulatory penalties for wrongful retention, but face no comparable penalty for wrongful delisting. This asymmetry makes over-compliance the rational operating posture: it is cheaper to delist a borderline or even clearly public-interest item than to defend retention through appeal. They administer the erasure queue and could build more rigorous public-interest review, but the cost of doing so falls on them while the benefit of getting it right accrues to third parties.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, search_and_platform_operators, agenda_setter,
    institutional, generational, constrained, continental).

% Published accurate, public-interest reporting on the erasure requester's conduct — court proceedings, regulatory sanctions, public statements — that subsequently becomes de-indexed or delisted following an erasure claim. They are rarely notified before delisting occurs and have limited, expensive avenues to contest removal. The practical effect is that their work becomes undiscoverable even though it was never found unlawful.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, continental).

% Maintain historical records, news archives, and public registries with a mission of long-term preservation. They lack the legal and financial resources to contest erasure claims individually and face reputational and resource pressure to comply quickly. Losing entries piecemeal degrades the historical record in ways that compound and are difficult to reverse once source links are broken.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    powerless, generational, trapped, continental).

% Rely on searchable public records to study corporate misconduct, professional accountability, and public figures' conduct over time. When records are delisted, their ability to establish patterns across time or across multiple actors is silently degraded, often without their awareness that anything has been removed.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers, payer,
    powerless, generational, constrained, continental).

% Nominally oversee the balance between erasure rights and freedom of expression, but in practice adjudicate individual complaints only after delisting has occurred and rarely proactively audit platform compliance patterns for public-interest overreach. Their formal role in weighing speech interests is largely reactive rather than structuring the initial decision.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, excluded,
    institutional, generational, analytical, continental).

% Private individuals seeking removal of genuinely irrelevant, outdated, or harmful personal information — the population the mechanism was designed to protect. They use the same erasure channel as bad-faith requesters and their legitimate claims are structurally indistinguishable at the point of filing, which is part of why the mechanism is hard to narrow without harming them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, ordinary_data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals a channel to remove outdated, irrelevant, or harmful personal information from search indexes and platforms without requiring litigation — solving a genuine problem of permanent digital exposure for minor or resolved personal matters.
% TRANSFER_FUNCTION: Moves control over public accessibility of factual, previously published information from publishers, archivists, and the public record to the individual named in that information, mediated by platform compliance decisions made under asymmetric liability pressure.
% ABSENT_VOICES: The public whose access to accurate historical and accountability information is diminished is diffuse and unorganized — no single reader discovers that a specific search result vanished, so no one is positioned to contest an individual delisting on behalf of public interest. Journalists and archivists are the closest organized proxies but are often not notified until after removal, if at all.
% DISAPPEARANCE_RATIONALE: For ordinary data subjects with genuine outdated-information claims, disappearance of the mechanism would restore permanent digital exposure with no clean remedy. For bad-faith requesters and reputation firms, disappearance would collapse a functioning suppression channel and their commercial and personal reputational strategies would need to shift to other means (SEO manipulation, legal threats). Journalists and archivists would see selectively suppressed records return to visibility. The world clearly rearranges for the suppression function even where legitimate privacy use would need a genuine, narrower replacement.
% FOUNDING_PROBLEM: Individuals had no mechanism to remove outdated, prejudicial, or resolved personal information (old minor convictions, youthful indiscretions, resolved financial difficulties) from indefinite, frictionless search discoverability, creating disproportionate lifelong harm from information that had lost its public relevance.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and the original Google Spain court reasoning attest the founding problem remains live for genuine cases of stale, irrelevant personal data. Journalism organizations, press freedom monitors, and independent researchers studying delisting transparency reports attest that the mechanism has been substantially captured for suppressing accurate, public-interest reporting about public conduct — a use case outside the founding problem's scope, corroborated by transparency-report analyses published independently of both platforms and requesters.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, contested).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__censorship_mechanism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__censorship_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71 at interval end) reflects the compounding effect of platforms' rational over-compliance: each individually low-stakes delisting decision aggregates into systemic removal of accurate public-interest content. Suppression (0.78) is high because the mechanism's persistence depends on the structural asymmetry in liability — platforms are punished for retention but not for wrongful removal, and courts/DPAs review only a small fraction of decisions after the fact. Theater ratio (0.42) captures that public-interest balancing tests exist on paper and are cited in transparency reports, but function largely as post-hoc justification for decisions already made under compliance pressure rather than as genuine ex ante filters.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters and reputation firms are coded as beneficiaries because the erasure channel functions, for them, as a low-cost mechanism to retract accurate information about themselves — the constraint subsidizes their reputational management. Journalists, archivists, and researchers are coded as victims because their published work and reference materials are removed from discoverability through a process they did not initiate and often cannot contest before the fact — high effective extraction despite formally 'neutral' process. Ordinary data subjects sit as beneficiaries structurally, per the founding intent, even though their claims are processed through the identical channel that bad-faith actors exploit — this is the mechanism's central design flaw under this reading: intake cannot distinguish the two populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting individuals from permanent digital exposure for stale, resolved matters) remains genuinely live for a subset of claimants, which prevents this reading from collapsing into a pure snare narrative without qualification — hence claimed_type is snare but the founding_problem_status is authored as contested, not dead. The mandatrophy signal here is narrower than full obsolescence: it is scope creep, where a legitimately-founded mechanism has been captured for a use (suppressing accurate public-interest reporting) well outside its founding justification, without the enforcement or adjudication apparatus evolving to exclude that use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intake_indistinguishability,
    'Can erasure-request intake structurally distinguish genuine stale-personal-data claims from bad-faith requests seeking to suppress accurate public-interest records, without imposing prohibitive review costs?',
    'Comparative analysis of jurisdictions or platforms that have implemented tiered review (e.g., mandatory public-interest flagging for claims involving public figures or matters of public record) against those using undifferentiated intake — measure delisting rates of investigative journalism content pre/post reform.',
    'If distinguishable at reasonable cost, this reading''s snare classification weakens toward tangled_rope (fixable coordination problem); if genuinely indistinguishable at intake, the suppression function is closer to structurally inherent to the mechanism as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intake_indistinguishability, empirical, 'Whether legitimate and bad-faith erasure claims can be separated procedurally.').

omega_variable(
    liability_asymmetry_origin,
    'Is the liability asymmetry (penalized for wrongful retention, not for wrongful delisting) an intentional legislative choice reflecting a genuine values priority, or an oversight in implementation that regulators could correct without new legislation?',
    'Legislative history review of Article 17 and its national implementations; comparison with jurisdictions that have added an explicit appeal-and-liability mechanism for wrongful delisting.',
    'If intentional, the suppression effect is a known and accepted tradeoff rather than a bug — reclassification toward tangled_rope becomes more defensible since the extraction is a foreseen cost of a genuine coordination choice. If an oversight, this reading''s snare framing is strengthened as an unintended, correctable capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liability_asymmetry_origin, conceptual, 'Whether the enforcement asymmetry driving suppression was a deliberate design tradeoff.').

omega_variable(
    kernel_framing_choice,
    'Is the censorship-mechanism framing the correct primary lens for Article 17''s operation as a whole, or does it capture only a minority, high-visibility tail of erasure requests while the bulk of requests are genuinely privacy-protective (the sibling privacy_fundamental_reading)?',
    'Platform transparency-report data on the proportion of erasure requests involving public figures, news content, or matters of public record versus purely private, non-newsworthy personal data.',
    'If the censorship use case is a small minority of total volume, this reading''s epsilon may be measuring a real but narrow phenomenon that should not be read as characterizing the kernel''s typical operation — reinforcing that this must remain a separate story from privacy_fundamental_reading rather than a modifier on it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framing: censorship use may be a minority tail rather than the dominant operating mode of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(arti_su_t24, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__censorship_mechanism_reading, 0.1).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, competitive_moat_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the article17_erasure_right kernel. privacy_fundamental_reading authors low extraction and a rope/mountain-adjacent structure for the same textual provision viewed as protecting genuine data sovereignty. competitive_moat_reading authors extraction concentrated on smaller platform competitors rather than on speech. All three share the kernel text but diverge in beneficiary/victim structure and epsilon; per the epsilon-invariance principle they are authored as three separate constraint stories linked by network edges rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
