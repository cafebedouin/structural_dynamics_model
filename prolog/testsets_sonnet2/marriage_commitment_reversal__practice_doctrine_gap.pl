% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Doctrine-Practice Gap in Plural Marriage Reversal (1890-1904)
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the marriage-commitment
 *   reversal kernel: that the structural feature doing the work between 1890
 *   and 1904 was neither a genuine revelatory reversal nor a pure external
 *   capitulation, but a maintained AMBIGUITY between preserved doctrine
 *   (Section 132, never formally repudiated as eternal principle) and
 *   suspended public practice (the 1890 Manifesto's compliance signal). Under
 *   this reading, the ambiguity itself is the constraint: it is what let the
 *   institution claim both continued fidelity to revealed truth (satisfying
 *   internal doctrinal commitments) and full compliance with federal law
 *   (satisfying external political survival needs) simultaneously, at the
 *   cost of clarity for ordinary members and at the cost of the
 *   fundamentalist remainder who took the undisavowed doctrine at face value.
 *   This is a different structural claim from the sibling readings — the
 *   endogenous reading treats Woodruff's vision as a genuine internal
 *   doctrinal event, and the exogenous reading treats Section 132 as fully
 *   preserved with practice simply halted by outside force. This reading's
 *   distinguishing claim is that the RELATIONSHIP between doctrine and
 *   practice was deliberately left unresolved, and that the resulting
 *   interpretive space was itself extracted value — approximately 200+ plural
 *   marriages performed in claimed-legal jurisdictions (Mexico, Canada)
 *   between 1890 and 1904 under this ambiguity, followed by a harder Second
 *   Manifesto in 1904 that finally closed the gap under renewed political
 *   pressure (the Smoot hearings).
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter/beneficiary (institutional/arbitrage) — administers the ambiguity and captures dual legitimacy
 *   - claimed_legal_jurisdiction_practitioners: beneficiary (moderate/constrained) — benefits from the gap while bearing legal exposure
 *   - general_membership: payer (powerless/trapped) — bears the confusion and later the sense of betrayal
 *   - fundamentalist_adherents: payer (organized/trapped) — bears schism and excommunication for taking preserved doctrine literally
 *   - federal_and_state_authorities: observer/excluded (institutional/analytical) — accepts the compliance signal without seeing the gap
 *   - second_manifesto_generation_1904: payer/excluded (powerless/trapped) — inherits the cost of closing the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Doctrine-Practice Gap in Plural Marriage Reversal (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '7e41b212-d1d1-428c-b18c-7e48fac8495b').
narrative_ontology:cs_kernel_codification('7e41b212-d1d1-428c-b18c-7e48fac8495b', fixed_text).
narrative_ontology:cs_authority_grounding('7e41b212-d1d1-428c-b18c-7e48fac8495b', lineage).
narrative_ontology:cs_interpretation_layer_present('7e41b212-d1d1-428c-b18c-7e48fac8495b').
narrative_ontology:cs_reading_relation('7e41b212-d1d1-428c-b18c-7e48fac8495b', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e41b212-d1d1-428c-b18c-7e48fac8495b', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('7e41b212-d1d1-428c-b18c-7e48fac8495b', foundational, doctrine_practice_gap_is_administered_not_incidental).
narrative_ontology:cs_axiom_status(doctrine_practice_gap_is_administered_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('7e41b212-d1d1-428c-b18c-7e48fac8495b', doctrine_practice_gap_is_administered_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('7e41b212-d1d1-428c-b18c-7e48fac8495b', secondary, ambiguity_persistence_past_founding_necessity_constitutes_extraction).
narrative_ontology:cs_axiom_status(ambiguity_persistence_past_founding_necessity_constitutes_extraction, holdable).
narrative_ontology:cs_axiom_grounding('7e41b212-d1d1-428c-b18c-7e48fac8495b', ambiguity_persistence_past_founding_necessity_constitutes_extraction, instrumental).
narrative_ontology:cs_reference_frame('7e41b212-d1d1-428c-b18c-7e48fac8495b', section_132_eternal_covenant_framework).
narrative_ontology:cs_drift_state('7e41b212-d1d1-428c-b18c-7e48fac8495b', post_1904_second_manifesto, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e41b212-d1d1-428c-b18c-7e48fac8495b', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, claimed_legal_jurisdiction_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, second_manifesto_generation_1904).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, section_132_eternal_validity).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the 1890 Manifesto as a public-compliance instrument while never repudiating Section 132 as revealed doctrine. Retains discretion to authorize, tolerate, or punish plural marriages case-by-case depending on jurisdiction and political exposure, and uses that discretion to preserve federal recognition, statehood prospects, and institutional survival while keeping the doctrinal claim to eternal truth intact for internal legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, church_institutional_leadership, beneficiary).

% Enter or continue plural marriages between 1890 and 1904 under leadership's tacit or explicit authorization, often in Mexico, Canada, or other jurisdictions read as outside the Manifesto's reach. Benefit from the ambiguity that lets them claim doctrinal fidelity and institutional cover simultaneously, at the cost of legal exposure if the ambiguity is later resolved against them.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, claimed_legal_jurisdiction_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% Told publicly that plural marriage has ended while privately aware or later discovering that select members continued practicing with leadership's knowledge. Experiences bewilderment and betrayal when the gap between public representation and internal practice becomes visible, with no forum to demand clarity because the ambiguity itself is never officially acknowledged as a policy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, trapped, national).

% Hold Section 132 as permanently binding doctrine and refuse to accept the practice suspension as authoritative, since the principle was never doctrinally revoked. Face excommunication and social exile from the mainstream institution for continuing what the preserved doctrine appears to command, becoming the schismatic remainder that absorbs the cost of the ambiguity's eventual resolution against continued practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_adherents, payer,
    organized, generational, trapped, regional).

% Accept the 1890 Manifesto as sufficient public compliance to grant statehood and restore civil rights, without full visibility into continued practice under claimed-legal jurisdictions. Would object to the doctrine-practice gap if it were transparent, but the ambiguity is structured precisely so that the compliance signal satisfies external scrutiny without requiring the internal contradiction to surface.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_and_state_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, federal_and_state_authorities, excluded).

% Inherits an institution that must issue a second, harder manifesto in 1904 (with excommunication penalties) to close the gap the first ambiguity opened. Bears the cost of the credibility repair, including purges of leaders who continued authorizing marriages, without having had any voice in the original ambiguous settlement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, second_manifesto_generation_1904, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, second_manifesto_generation_1904, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine-practice gap coordinates the institution's need to signal compliance to external federal authority while preserving internal doctrinal continuity for a membership base that had been taught plural marriage was a permanent, eternal principle — a genuine transition problem given the magnitude of the reversal being asked of a committed membership.
% TRANSFER_FUNCTION: Moves interpretive certainty and membership trust away from ordinary members and toward institutional leadership's discretionary control; moves legal and reputational risk onto practitioners who continue under claimed-legal jurisdictions and onto fundamentalists who are later expelled; concentrates the benefit of preserved doctrinal legitimacy plus preserved federal standing in the institution itself.
% ABSENT_VOICES: Rank-and-file members who entered plural marriages in good faith before 1890 and their families had no voice in how the ambiguity would be administered case-by-case; fundamentalist adherents who took Section 132 at its word were not consulted before being recast as apostate for continuing what doctrine still affirmed.
% DISAPPEARANCE_RATIONALE: If the ambiguity had not existed — if the 1890 Manifesto had either fully repudiated Section 132 or the practice had continued fully unrestricted — the institution would have faced either the immediate mass schism the ambiguity delayed, or the immediate federal dissolution the ambiguity avoided. The gap itself is load-bearing: removing it collapses the dual-track arrangement that let both external compliance and internal continuity claims coexist for fourteen years.
% FOUNDING_PROBLEM: The institution faced federal seizure of assets, disincorporation, and disenfranchisement of practicing members under escalating anti-polygamy legislation, while having taught for decades that plural marriage was a divinely commanded, eternally binding principle that could not simply be declared false.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the institution (drawing on court records, correspondence, and the 1904 Smoot hearings testimony) attest that the federal pressure was resolved well before 1904 and that continued authorization of marriages after 1890 was a matter of institutional discretion rather than survival necessity; the institution's own 1904 Second Manifesto and subsequent excommunications of authorizing leaders constitute an internal acknowledgment that the gap had outlived any defensible transitional justification.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78) because the ambiguity is not incidental confusion but a structurally maintained interpretive space that the institution actively administers for its own benefit — it is what allows continued marriages under claimed-legal jurisdictions while the public narrative asserts cessation. Suppression is moderate-high (0.62) and rises over the interval as the institution must increasingly police who is permitted to know about, or benefit from, the gap, culminating in the 1904 crackdown. Theater ratio is authored substantial and rising (peaking near 0.65 before the 1904 correction) because an increasing share of institutional communication during this period is performative reassurance to both external and internal audiences that no gap exists, while the practice underneath continues. Accessibility collapse is moderate (0.45) — the gap is real and exploitable knowledge, not a fully closed alternative space; some members did have access to what was happening, which is part of what generates fundamentalist resistance rather than uniform compliance. Resistance is high (0.71) reflecting the schism, congressional hearings, and eventual internal purge the ambiguity provoked.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the gap is a defensible, even necessary, exercise of prophetic discretion managing an existential threat during a genuine transition. From the general membership and fundamentalist seats, the identical structure is experienced as either bewildering (general membership, discovering the gap after the fact) or as betrayal of a permanent commandment (fundamentalists, for whom Section 132's non-repudiation is decisive). The engine should compute divergent per-seat types from this same structural data: the agenda_setter's seat likely computes closer to coordinated scaffold-like management, while the payer seats compute closer to tangled_rope or snare given the asymmetric extraction of clarity and the eventual excommunication costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership is coded as full beneficiary/agenda_setter: it authors, administers, and profits from the ambiguity in the form of preserved doctrinal legitimacy plus federal recognition. Claimed-legal jurisdiction practitioners are secondary beneficiaries — they get to continue a practice they believe divinely commanded — but their exit options are constrained because continuing exposes them to eventual excommunication once the second manifesto closes the gap (as later happened to several authorizing leaders). General membership and fundamentalist adherents are coded as high-d targets: general membership because they are trapped in an information asymmetry not of their making, fundamentalists because their trapped/organized position (real communal structure, no legitimate exit within the tradition without full excommunication) means the eventual resolution against them is total institutional and social cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal dissolution threat) is authored as dead by 1904 per external corroboration, yet the ambiguity persisted for fourteen years after the 1890 Manifesto ostensibly resolved it — this is a textbook mandatrophy signature: an arrangement whose original survival-necessity function had been substantially achieved (statehood negotiations proceeding, disincorporation reversed) continuing to operate under its original justification for over a decade because leadership found the discretionary flexibility valuable independent of the original threat. Classifying this as tangled_rope rather than pure snare recognizes that a genuine coordination problem existed at the founding moment (mass membership could not simply be told a permanent divine commandment was false overnight without catastrophic schism) — the extraction is the surplus flexibility retained past the point the coordination problem was resolved, not the entire arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gap_intentional_vs_emergent,
    'Was the doctrine-practice gap a deliberately engineered institutional strategy, or did it emerge unplanned from the collision of two genuinely held but incompatible commitments (revealed permanent doctrine vs. survival necessity) that leadership never consciously decided to leave ambiguous?',
    'Close reading of contemporaneous leadership correspondence, journal entries, and council minutes (to the extent available) from 1890-1904 for evidence of explicit strategic discussion of maintaining ambiguity versus evidence of leadership itself being internally divided or uncertain.',
    'If deliberately engineered, this reading''s tangled_rope/high-extraction classification is strongly supported. If emergent from genuine internal disagreement among leadership about which sibling reading was true, the constraint may be better modeled as an unstable transitional state rather than an administered extraction mechanism, softening the extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gap_intentional_vs_emergent, empirical, 'Whether the practice-doctrine gap was strategically administered or an unplanned byproduct of internal leadership disagreement.').

omega_variable(
    which_reading_is_true_kernel_indeterminacy,
    'Is the marriage_commitment_reversal kernel genuinely irreducible to a single reading — i.e., did the historical event itself instantiate all three structural possibilities (genuine revelation, external coercion, AND administered ambiguity) simultaneously for different actors — or does one reading have superior claim to describing what actually happened?',
    'This is the committer-frame ambiguity itself: per the ε-invariance principle, each reading is authored as its own constraint with its own ε rather than resolved into a single value. Resolution would require abandoning the multi-reading approach in favor of declaring one reading canonical, which this framework deliberately does not do.',
    'If a future historiographical consensus strongly favors one reading (e.g., overwhelming evidence that Woodruff''s revelation was sincere and doctrinally complete, closing the gap immediately), this practice_doctrine_gap reading''s beneficiary/victim structure would need re-examination — the ambiguity would be revealed as illusory rather than structurally load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_true_kernel_indeterminacy, conceptual, 'Whether the three kernel readings describe genuinely coexisting structural facts or competing historical claims where one should dominate.').

omega_variable(
    fundamentalist_schism_causal_weight,
    'How much of the fundamentalist schism (Mormon fundamentalism as an ongoing movement) is directly attributable to this specific doctrine-practice gap, versus attributable to the harder, unambiguous 1904 Second Manifesto and subsequent excommunications?',
    'Comparative analysis of fundamentalist movement founding narratives and self-justifications: do they cite the 1890-1904 gap period specifically, or primarily the 1904 closure and post-1904 excommunications as the precipitating grievance?',
    'If the schism is primarily a 1904-closure phenomenon rather than a 1890-1904 gap phenomenon, victim attribution for fundamentalist_adherents in this story''s interval (1890-1904) should be reduced, with more weight shifted to a potential separate constraint story about the 1904 Second Manifesto itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_schism_causal_weight, empirical, 'Whether fundamentalist schism costs belong to the ambiguity period or the closure event.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1892, 0.42).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.48).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1896, 0.53).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.58).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1900, 0.62).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1902, 0.65).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1892, 0.62).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.68).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1896, 0.72).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.75).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1900, 0.77).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1902, 0.79).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1892, 0.45).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.5).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1896, 0.53).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.56).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1900, 0.58).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1902, 0.6).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal_endogenous_reinterpretation).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal_exogenous_override).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the marriage_commitment_reversal kernel, each authored as a structurally distinct constraint per the ε-invariance principle rather than as one constraint with a contested observable. The endogenous_reinterpretation reading treats Woodruff's 1890 vision as genuine internal doctrinal revision (lower extraction — coordinated theological adaptation). The exogenous_override reading treats the reversal as pure external coercion with Section 132 fully intact (extraction directed at federal power's coercive capacity rather than at membership). This practice_doctrine_gap reading treats the RELATIONSHIP between doctrine and practice as itself the extracted resource, independent of which underlying causal story is true, producing the highest ε of the three because it identifies an administered ambiguity sustained past its founding necessity, extracting clarity from general membership and eventually schism cost from fundamentalists. All three should be read together as decompositions of the colloquial single label 'the 1890 Manifesto reversal.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
