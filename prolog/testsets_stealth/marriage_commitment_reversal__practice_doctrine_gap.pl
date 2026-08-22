% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Post-Manifesto Practice-Doctrine Gap: Section 132 Preserved in Canon, Compliance Performed in Public
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   Following the October 1890 Manifesto, the church's canonized marriage
 *   revelation (Doctrine and Covenants 132) remained in force as doctrine
 *   while public practice was suspended for legal compliance. Between 1890
 *   and 1904, more than two hundred plural marriages were performed under
 *   presidential authorization in jurisdictions the leadership judged outside
 *   United States law — the Mormon colonies in northern Mexico, southern
 *   Alberta, and international waters — while public answers to press and
 *   Senate inquiry denied any continuation. The arrangement was a dual-track
 *   legitimation structure: a compliance performance addressed to the nation,
 *   and a continuity track addressed to the covenant. Exposure at the Reed
 *   Smoot hearings (1903-1907) forced closure via the Second Manifesto
 *   (1904), discipline of two apostles, and the schismatic exit of members
 *   who took the preserved doctrine literally. This file instantiates ONE
 *   reading of the marriage_commitment_reversal kernel — the structural-gap
 *   reading — as a clean epsilon-invariant constraint; the endogenous
 *   (revelation) and exogenous (coercion) readings are separate constraints
 *   with their own epsilon values, linked through the network block. Claim
 *   and metrics are independent authored facts: the tangled_rope claim states
 *   what I believe is structurally true; the metrics state what I believe is
 *   descriptively true of the arrangement's operation.
 *
 * KEY AGENTS:
 *   - institutional_leadership: agenda-setting beneficiary (institutional/identity_locked) — administers both tracks, collects institutional continuity and discretionary control
 *   - colony_sealing_officiants: beneficiary-executants (powerful/identity_locked) — perform authorized sealings abroad, carry personal exposure
 *   - post_manifesto_sealing_recipients: beneficiary-payers (moderate/constrained) — receive the ordinance, bear secrecy burden and later repudiation risk
 *   - general_membership: primary payers (organized/identity_locked) — sold compliance as total, absorb betrayal costs on disclosure
 *   - fundamentalist_dissidents: payers via schism (powerless/constrained) — take the preserved doctrine literally, are expelled
 *   - antipolygamy_enforcement_authorities: excluded coercive counterparty (institutional/mobile) — the audience of the compliance performance, deliberately kept outside the private track
 *   - institutional_historians: analytical observers (analytical/analytical) — reconstruct the authorization chain from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.75).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.73).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.57).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.57).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Post-Manifesto Practice-Doctrine Gap: Section 132 Preserved in Canon, Compliance Performed in Public").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, 'c1711d94-ec55-4830-b75d-4a930e6f15a2').
narrative_ontology:cs_kernel_codification('c1711d94-ec55-4830-b75d-4a930e6f15a2', fixed_text).
narrative_ontology:cs_authority_grounding('c1711d94-ec55-4830-b75d-4a930e6f15a2', lineage).
narrative_ontology:cs_interpretation_layer_present('c1711d94-ec55-4830-b75d-4a930e6f15a2').
narrative_ontology:cs_reading_relation('c1711d94-ec55-4830-b75d-4a930e6f15a2', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1711d94-ec55-4830-b75d-4a930e6f15a2', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('c1711d94-ec55-4830-b75d-4a930e6f15a2', foundational, section_132_remains_binding_law).
narrative_ontology:cs_axiom_status(section_132_remains_binding_law, holdable).
narrative_ontology:cs_axiom_grounding('c1711d94-ec55-4830-b75d-4a930e6f15a2', section_132_remains_binding_law, theological).
narrative_ontology:cs_axiom('c1711d94-ec55-4830-b75d-4a930e6f15a2', foundational, administrative_suspension_cannot_revoke_revelation).
narrative_ontology:cs_axiom_status(administrative_suspension_cannot_revoke_revelation, holdable).
narrative_ontology:cs_axiom_grounding('c1711d94-ec55-4830-b75d-4a930e6f15a2', administrative_suspension_cannot_revoke_revelation, conventional).
narrative_ontology:cs_reference_frame('c1711d94-ec55-4830-b75d-4a930e6f15a2', section_132_permanent_binding_principle).
narrative_ontology:cs_drift_state('c1711d94-ec55-4830-b75d-4a930e6f15a2', manifesto_through_smoot_hearings_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c1711d94-ec55-4830-b75d-4a930e6f15a2', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, colony_sealing_officiants).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_sealing_recipients).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_sealing_recipients).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, section_132_eternal_marriage_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_discretion_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The First Presidency and Quorum of the Twelve administer the church after the October 1890 announcement. They issue the public statement suspending plural marriage, answer Senate and press inquiries with denials of further ceremonies, and privately authorize selected sealings in northern Mexico, southern Alberta, and aboard international waters where they judge United States law does not reach. They decide which petitions are granted, keep the authorization chain out of published records, and weigh each exposure risk against the community's legal standing. Abandoning either course entirely — renouncing the founding marriage revelation or resuming open ceremony — would cost them respectively the community's theological foundation or its legal existence.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Apostles resident in the Mormon colonies of Chihuahua and the Canadian settlement perform the marriages referred to them under presidential authorization. They keep no published registers, instruct participants in discretion, and rely on distance from United States marshals. They retain access to the ordinance they regard as eternally required, and carry personal arrest and disciplinary exposure if the chain of authorizations surfaces.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, colony_sealing_officiants, beneficiary,
    powerful, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, colony_sealing_officiants, agenda_setter).

% Couples sealed between 1890 and 1904 receive the ordinance they believe essential for their family's eternal standing. In exchange they accept secrecy instructions, potential exposure before federal inquiries, and — after 1904 — the possibility that their marriages are publicly repudiated, their sealings discontinued, and their families declared irregular by the same authority that authorized them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_sealing_recipients, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_sealing_recipients, payer).

% Rank-and-file Latter-day Saints are told the practice has ended. They fund the recovery of confiscated property, celebrate Utah statehood, and arrange their family plans around the announced policy. When Senate testimony reveals continuing ceremonies, many experience the disclosure as deception by leaders they regarded as prophets. Their standing in the community, their eternal-family expectations, and their entire social world depend on remaining within it.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    organized, generational, identity_locked, continental).

% Members who read the preserved revelation as still-commanding continue or seek the practice, first in the colonies and later in clandestine circles. They face excommunication, loss of temple access, and social severance. After 1904 the main body disciplines them out, and their congregations become the seed of separate fundamentalist churches that claim the original covenant.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_dissidents, payer,
    powerless, generational, constrained, regional).

% Federal marshals, prosecutors operating under the Edmunds and Edmunds-Tucker Acts, and later the Senate committee investigating Reed Smoot's seating pursue unlawful cohabitation and polygamous ceremonies and demand complete cessation. They are told compliance is total; they are not told of colonial authorizations. When hearings surface the hidden record, they treat the earlier denials as obstruction, and their escalated demands close the arrangement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, antipolygamy_enforcement_authorities, excluded,
    institutional, biographical, mobile, national).

% Later scholars working from diaries, sealing records, colony registers, and hearing transcripts reconstruct the authorization chain and estimate the size of the post-announcement marriage population. They hold no stake in either the community's standing or the prosecutions, and can set the public record beside the private one.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dual-track arrangement solved a concrete collective problem: a covenant community facing disincorporation, property seizure, and statehood conditionality needed to restore legal standing without repudiating the revelation its members believed binding. It coordinated the community's external presentation (public compliance) with its internal continuity (canonized principle retained, selected ordinances continued under claimed-foreign jurisdictions).
% TRANSFER_FUNCTION: Moves certainty and decision rights from the general membership to the institutional leadership, which alone knows which marriages proceed and under what cover; moves legal and reputational risk onto officiants and sealing recipients; moves legitimacy capital from prophetic infallibility toward administrative prudence; and, on disclosure, moves betrayal costs onto the whole membership and schism costs onto dissenters.
% ABSENT_VOICES: Federal prosecutors and the Smoot committee were structurally kept outside the private track — the arrangement worked precisely because they were not in the room, and when they were admitted (1903-1907) it collapsed. Rank-and-file members who would have demanded honesty had they known of the authorizations had no seat; fundamentalist objectors who argued that suspension without revocation was incoherent were marginalized and then expelled.
% DISAPPEARANCE_RATIONALE: If the dual track vanished overnight in 1890, the community faced an immediate forced choice: open defiance (resumption of prosecutions, disincorporation, asset loss, imprisonment of leadership) or doctrinal repudiation (rupture of prophetic legitimacy and mass disillusionment). The colony settlements, the statehood strategy, and the property-recovery program were all arranged around the gap's continued operation.
% FOUNDING_PROBLEM: How a community whose constitutive covenant practice had been criminalized — under federal statutes disincorporating the church and escheating its property — could retain legal existence without repudiating the revelation its members held binding.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by congressional hearing testimony (Reed Smoot proceedings, 1903-1907), federal court dockets under the Edmunds legislation, contemporaneous opposition press (Salt Lake Tribune), and the later testimony and discipline of apostles John W. Taylor and Matthias F. Cowley. Leadership's own public statements during the interval denied that any gap existed, so no beneficiary attests the arrangement's operation while it runs; the documentary record exists because adversaries compelled it.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends high (0.75) because the arrangement's operation transferred clarity from members to leadership, transferred legal risk to officiants and recipients, and culminated in betrayal costs imposed on the entire membership when the hidden track surfaced. Suppression is high (0.73) because persistence required active maintenance: sworn-adjacent public denials, unpublished authorization chains, coached discretion, and eventual discipline of dissenters — the suppression_requirement series is authored because enforcement capacity visibly matured and hardened across the interval as exposure risk grew, which is precisely the dynamic the scalar cannot capture. Theater ratio ends at 0.57: by interval end the majority of publicly visible activity was compliance performance, while the arrangement's substantive function ran through unpublished channels; early in the interval the Manifesto carried substantial sincere content (genuine relief from persecution), so theater starts lower and climbs. Accessibility_collapse is 0.62 — exit, open dissent, and alternative interpretation were largely closed to embedded members (community, theology, and family expectations all hung on staying), but the fundamentalist exit demonstrates alternatives were not fully collapsed. Resistance is 0.45 — apostolic refusal to sustain the Second Manifesto, fundamentalist persistence, adversarial press scrutiny, and the Senate investigation itself. All three metric series run on one shared eight-point grid (1890-1904, biennial) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience radically different arrangements under identical structure. From the leadership seat this is necessary stewardship — prudence that saved the community when principle and law collided. From the membership seat it is prophetic integrity that becomes betrayal on disclosure. From the fundamentalist seat it is incoherence demanding schism: if the doctrine binds, the suspension is apostasy; if it does not, the canon is false. From the enforcement seat, once informed, it is obstruction under oath. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and colony officiants sit near the beneficiary end: they collect continuity, discretion, and ordinance access, and their costs (exposure risk, eventual discipline) are real but subordinate to what the arrangement preserves for them. General membership and fundamentalist dissidents sit near the target end: membership pays in deceived certainty and betrayed trust; dissidents pay in expulsion and severed salvation claims. Post-manifesto sealing recipients are genuinely dual-positioned — the derivation from their beneficiary declaration alone would place them near the full-beneficiary end, understating their secrecy burden, legal jeopardy, and post-1904 repudiation exposure, hence the explicit override for the moderate power atom (d = 0.35). Antipolygamy enforcement authorities are excluded rather than coordinated: their exclusion is the enforcement object itself, and their eventual admission to the conversation is what terminates the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric misreadings. A pure-extraction reading (snare) misses the genuine coordination function: most members initially welcomed the Manifesto as relief from persecution, and the arrangement demonstrably solved a real survival problem — the community regained legal standing, recovered assets, and achieved statehood. A pure-coordination reading (rope) misses the asymmetric extraction: clarity, risk, and schism costs fell on seats that never set the agenda. Holding both halves is what the hybrid category exists for. On mandatrophy: the founding problem (criminalized constitutive practice) remained live throughout the interval and was resolved by termination, not decay — the Second Manifesto closed the gap rather than letting it atrophy into theatrical maintenance, so this story shows no piton trajectory; the arrangement ended while its function was still contested, which is why the victim set includes a schism rather than a residue of inertial compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the practice-doctrine gap a distinct, administered structural arrangement (this reading), or merely the observable residue of either sincere revelation (endogenous sibling) or bare coercion (exogenous sibling)?',
    'Archival adjudication: dated presidential authorizations, Woodruff-era diaries, and the sequence of the vision claim against policy decisions would show whether the ambiguity was designed and managed or emergent from a simpler cause.',
    'If the endogenous reading is adopted, extractiveness drops materially (no deception intended, doctrine genuinely developed); if the exogenous reading is adopted, the arrangement''s ownership shifts to the coercing sovereign and the church''s seat reclassifies toward pressured compliance rather than administration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the marriage_commitment_reversal kernel the historical record best supports, and what each adoption does to epsilon and seat ownership.').

omega_variable(
    post_manifesto_marriage_count,
    'How many plural marriages were actually performed between 1890 and 1904, and how many carried documented presidential authorization versus opportunistic performance?',
    'Archival audit of sealing records, Mexican and Canadian colony registers, and exhibits introduced at the Smoot hearings, cross-checked against the Derr et al. and Daynes counts.',
    'A higher authorized count raises epsilon and enlarges the recipient victim-beneficiary population; a lower count narrows the gap and weakens the dual-track reading in favor of isolated exceptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_marriage_count, empirical, 'Scale and authorization coverage of the post-Manifesto marriage population.').

omega_variable(
    dual_track_intentionality,
    'Was the dual-track legitimation structure a deliberate strategy adopted at the top, or an emergent compromise that accreted through local decisions in the colonies?',
    'Decision-point reconstruction: trace whether each post-1890 authorization traces to written presidential approval or to officiant initiative later ratified, using the Joseph F. Smith papers and colony correspondence.',
    'Deliberate design supports a tightly administered hybrid with high theater and concentrated agenda-setting; emergent drift weakens the agenda_setter attribution and pushes the arrangement toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_track_intentionality, conceptual, 'Whether the ambiguity was architected or accreted.').

omega_variable(
    suppression_mechanism_mix,
    'Is member quiescence during the interval primarily structural (legal jeopardy, disciplinary threat, total social embedding) or internalized (trust in prophetic authority that made doubt feel like apostasy)?',
    'Post-1904 trajectory: if deference patterns and self-blame persisted after the Second Manifesto clarified the record, the internalized share is substantial; if bewilderment resolved quickly into institutional criticism, the structural share dominates.',
    'If internalized, the arrangement''s effective suppression exceeds the structural measure — members carried the enforcement mechanism with them, and the betrayal costs on disclosure were correspondingly deeper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mix, empirical, 'Structural versus internalized components of the measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_gap_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.3).
narrative_ontology:measurement_basis(mcr_gap_tr_t1890, observed).
narrative_ontology:measurement(mcr_gap_tr_t1892, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1892, 0.32).
narrative_ontology:measurement_basis(mcr_gap_tr_t1892, observed).
narrative_ontology:measurement(mcr_gap_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.36).
narrative_ontology:measurement_basis(mcr_gap_tr_t1894, observed).
narrative_ontology:measurement(mcr_gap_tr_t1896, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1896, 0.4).
narrative_ontology:measurement_basis(mcr_gap_tr_t1896, observed).
narrative_ontology:measurement(mcr_gap_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.44).
narrative_ontology:measurement_basis(mcr_gap_tr_t1898, observed).
narrative_ontology:measurement(mcr_gap_tr_t1900, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1900, 0.48).
narrative_ontology:measurement_basis(mcr_gap_tr_t1900, observed).
narrative_ontology:measurement(mcr_gap_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.52).
narrative_ontology:measurement_basis(mcr_gap_tr_t1902, observed).
narrative_ontology:measurement(mcr_gap_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.57).
narrative_ontology:measurement_basis(mcr_gap_tr_t1904, observed).

% Extraction over time
narrative_ontology:measurement(mcr_gap_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.52).
narrative_ontology:measurement_basis(mcr_gap_be_t1890, observed).
narrative_ontology:measurement(mcr_gap_be_t1892, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1892, 0.56).
narrative_ontology:measurement_basis(mcr_gap_be_t1892, observed).
narrative_ontology:measurement(mcr_gap_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.6).
narrative_ontology:measurement_basis(mcr_gap_be_t1894, observed).
narrative_ontology:measurement(mcr_gap_be_t1896, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1896, 0.64).
narrative_ontology:measurement_basis(mcr_gap_be_t1896, observed).
narrative_ontology:measurement(mcr_gap_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.67).
narrative_ontology:measurement_basis(mcr_gap_be_t1898, observed).
narrative_ontology:measurement(mcr_gap_be_t1900, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement_basis(mcr_gap_be_t1900, observed).
narrative_ontology:measurement(mcr_gap_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.72).
narrative_ontology:measurement_basis(mcr_gap_be_t1902, observed).
narrative_ontology:measurement(mcr_gap_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.75).
narrative_ontology:measurement_basis(mcr_gap_be_t1904, observed).

% Suppression requirement over time
narrative_ontology:measurement(mcr_gap_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement_basis(mcr_gap_su_t1890, observed).
narrative_ontology:measurement(mcr_gap_su_t1892, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1892, 0.52).
narrative_ontology:measurement_basis(mcr_gap_su_t1892, observed).
narrative_ontology:measurement(mcr_gap_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.55).
narrative_ontology:measurement_basis(mcr_gap_su_t1894, observed).
narrative_ontology:measurement(mcr_gap_su_t1896, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1896, 0.58).
narrative_ontology:measurement_basis(mcr_gap_su_t1896, observed).
narrative_ontology:measurement(mcr_gap_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.61).
narrative_ontology:measurement_basis(mcr_gap_su_t1898, observed).
narrative_ontology:measurement(mcr_gap_su_t1900, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement_basis(mcr_gap_su_t1900, observed).
narrative_ontology:measurement(mcr_gap_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.69).
narrative_ontology:measurement_basis(mcr_gap_su_t1902, observed).
narrative_ontology:measurement(mcr_gap_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.73).
narrative_ontology:measurement_basis(mcr_gap_su_t1904, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 reversal of plural marriage' conflates three structurally distinct claims: a revelatory revision (endogenous_reinterpretation_reading), a coerced suspension with doctrine formally intact (exogenous_override_reading), and a managed ambiguity in which doctrine and practice decouple into parallel legitimation tracks (this file). Per the epsilon-invariance principle these are three constraints, not one constraint viewed from three angles: each carries its own epsilon, victim set, and enforcement profile. This reading sits downstream of the exogenous account, which supplies the coercive trigger this arrangement metabolizes, and competes with the endogenous account, which denies the gap exists by attributing the change to revelation itself. Family members are linked through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
