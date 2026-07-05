% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference to Congressional Copyright Term-Setting via Rational Basis Review
 *   domain: constitutional_law/intellectual_property
 *
 * SUMMARY:
 *   This story isolates the judicial posture toward copyright term
 *   legislation, not the term length itself or the public-good theory of
 *   copyright's origin. The constraint here is the specific doctrinal move:
 *   courts (paradigmatically the Supreme Court in Eldred v. Ashcroft) treat
 *   'for limited Times' as satisfied by any finite term, however extended,
 *   and apply rational basis review rather than a purposive or searching
 *   standard. This creates a procedural channel through which Congress can
 *   convert an originally scaffolded, purpose-bound grant (see the sibling
 *   public_scaffold_reading) into something functionally closer to indefinite
 *   corporate enclosure (see the sibling corporate_enclosure_reading) without
 *   ever triggering constitutional invalidation. The judicial ambiguity
 *   reading is the hinge: it does not itself decide whether copyright IS a
 *   scaffold or an enclosure — it decides who gets to decide, and insulates
 *   that decision from judicial correction. Theater ratio rises sharply
 *   around 1998 (Sonny Bono Act, Eldred litigation) as the doctrinal
 *   apparatus of deference is elaborated and cited with increasing formality
 *   even as the underlying 'promote progress' inquiry becomes more clearly
 *   vestigial.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Congressional Copyright Term-Setting via Rational Basis Review").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "constitutional_law/intellectual_property").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, 'dea7006d-1980-4458-af51-e0a57ace3ff0').
narrative_ontology:cs_kernel_codification('dea7006d-1980-4458-af51-e0a57ace3ff0', fixed_text).
narrative_ontology:cs_authority_grounding('dea7006d-1980-4458-af51-e0a57ace3ff0', practice).
narrative_ontology:cs_interpretation_layer_present('dea7006d-1980-4458-af51-e0a57ace3ff0').
narrative_ontology:cs_reading_relation('dea7006d-1980-4458-af51-e0a57ace3ff0', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('dea7006d-1980-4458-af51-e0a57ace3ff0', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_axiom('dea7006d-1980-4458-af51-e0a57ace3ff0', foundational, term_setting_is_a_political_question).
narrative_ontology:cs_axiom_status(term_setting_is_a_political_question, holdable).
narrative_ontology:cs_axiom_grounding('dea7006d-1980-4458-af51-e0a57ace3ff0', term_setting_is_a_political_question, conventional).
narrative_ontology:cs_axiom('dea7006d-1980-4458-af51-e0a57ace3ff0', foundational, finite_term_satisfies_limited_times_regardless_of_length).
narrative_ontology:cs_axiom_status(finite_term_satisfies_limited_times_regardless_of_length, holdable).
narrative_ontology:cs_axiom_grounding('dea7006d-1980-4458-af51-e0a57ace3ff0', finite_term_satisfies_limited_times_regardless_of_length, conventional).
narrative_ontology:cs_reference_frame('dea7006d-1980-4458-af51-e0a57ace3ff0', separation_of_powers_deference_baseline).
narrative_ontology:cs_drift_state('dea7006d-1980-4458-af51-e0a57ace3ff0', post_sonny_bono_eldred_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dea7006d-1980-4458-af51-e0a57ace3ff0', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_claimants).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_a_constraint).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, authors_and_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, authors_and_creators).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, legislative_primacy_in_ip_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length through ordinary legislation, repeatedly extending terms (1909, 1976, 1998) without ever triggering meaningful judicial scrutiny of whether the extensions still satisfy 'limited Times.' Because courts read the constitutional text as delegating discretion, Congress can respond to lobbying pressure from rightsholders without needing to justify the extension against the constitutional purpose. Its exit option from any constraint is essentially unlimited: it can simply legislate again.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Lobbies for term extensions (the Sonny Bono Act is the paradigm case) and captures the economic value of works that would otherwise enter the public domain. Benefits directly from the judicial posture that treats term length as a legislative policy question rather than a constitutional boundary subject to independent judicial measurement.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holding_industries, beneficiary,
    organized, generational, arbitrage, national).

% In Eldred v. Ashcroft (2003), declined to treat 'limited Times' as a judicially enforceable ceiling, holding that as long as the term is literally finite in number, Congress's discretion to extend it retroactively is a rational policy choice owed deference. The Court could have adopted a more searching standard but chose not to; this choice is itself the constraint being classified. It bears none of the costs of the deference it authored.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, supreme_court, observer).

% Includes libraries, archivists, remix artists, educators, and the general public who would gain free use of works upon expiration of copyright. Each extension defers entry into the public domain for an entire generation of works. This group is diffuse, unorganized, and structurally unable to litigate the term question again after Eldred foreclosed the argument; their only recourse is a new constitutional theory or a legislative reversal neither is forthcoming.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_claimants, payer,
    powerless, generational, trapped, national).

% Individual creators sometimes benefit from longer terms (heirs collect royalties longer) but are structurally distinct from the corporate rightsholders who lobby for extension; most individual authors' works have negligible commercial value by the time extensions matter, so the benefit is nominal while they also pay indirectly as consumers and remixers of a shrinking public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, authors_and_creators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, authors_and_creators, payer).

% The clause itself has no voice in the proceeding that determines its own meaning; its textual constraint ('for limited Times') is read by the reviewing court as satisfied by any term that is finite in principle, however long in practice, which empties the word 'limited' of independent constraining force.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_text_limited_times_clause, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_text_limited_times_clause).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rational basis review coordinates the separation of powers: it allocates the term-setting decision to the politically accountable branch (Congress) rather than the judiciary, avoiding a court substituting its own policy judgment about optimal copyright duration for the legislature's.
% TRANSFER_FUNCTION: The deference standard transfers effective control over the pace of public domain entry from the judiciary (interpreting a constitutional ceiling) to Congress (subject to industry lobbying), and by extension moves economic value from the public domain to concentrated rightsholders whenever Congress extends terms.
% ABSENT_VOICES: Future generations who would have benefited from works entering the public domain on the original schedule have no seat in the litigation or the legislative process; present-day libraries and archives object but carry far less lobbying weight than incumbent rightsholders, and the Eldred plaintiffs already tried and lost the doctrinal argument that would have given them standing to object more forcefully.
% DISAPPEARANCE_RATIONALE: If judicial deference were withdrawn and courts instead applied a searching, purpose-driven standard to 'limited Times' (asking whether an extension actually promotes the constitutional purpose of progress, and refusing retroactive extensions that only enrich existing holders), Congress's capacity to extend terms indefinitely via incremental legislation would be constrained for the first time since the Republic's founding, materially changing the pace of public domain growth and the economics of the copyright lobby.
% FOUNDING_PROBLEM: The Constitution enumerates a specific, purposive power ('to promote the Progress of Science') exercised through a specific, textually limited mechanism ('for limited Times') — the deference doctrine was built to prevent courts from second-guessing Congress's line-drawing on a policy question (how long is long enough) that seems inherently legislative and cannot be answered by pure legal reasoning.
% FOUNDING_PROBLEM_CORROBORATION: The Court and Congress both attest the deference framework remains necessary to preserve separation of powers. Outside the benefiting parties, legal scholars (e.g., Lawrence Lessig, who argued Eldred) and economists studying optimal copyright duration have published analyses arguing the term length has drifted from any plausible incentive-based justification, and dissenting Justice Breyer's Eldred dissent itself, from inside the Court but outside the majority coalition, documented the economic case that the extension served no discernible 'progress' purpose.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.42 at plateau) because the deference doctrine itself does not extract value — it is a procedural allocation of decision-making authority. The extraction happens downstream, through the term extensions the doctrine permits, which is why this constraint's ε is meaningfully lower than what a story about the term extensions themselves would show (that would be the corporate_enclosure_reading's domain). Theater ratio is comparatively high (0.55) because the judicial opinions maintaining deference perform extensive textual and purposive analysis while reaching a conclusion (any finite term is 'limited') that renders the analysis largely decorative — the word 'limited' does no independent constraining work once satisfied by any large finite number. Suppression is moderate (0.35): the doctrine does not use coercive enforcement in the ordinary sense, but it does foreclose a class of constitutional arguments (Eldred's plaintiffs cannot re-litigate the same theory), which is a real, if soft, suppression of future challenge.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority and the Court itself are the structural beneficiaries of this doctrine — Congress gains untrammeled discretion, and the Court gains a workable exit from having to draw an inherently line-drawing-heavy substantive rule. Copyright holding industries are downstream beneficiaries who capture the economic value the doctrine enables Congress to preserve for them. Public domain claimants are the diffuse victims: no single one of them loses enough to justify individual litigation, and Eldred foreclosed the doctrinal path for organized challenge. 'Constitutional fixity as a constraint' is listed as a victim in a more abstract sense — the text's power to independently bind Congress is what erodes under this reading, distinct from any particular human party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding judicial usurpation of an inherently legislative line-drawing function) remains genuinely live in the abstract — courts should not be setting copyright terms by judicial fiat. But the doctrine as currently applied has drifted from 'defer to Congress's reasonable line-drawing' to 'defer to Congress's line-drawing regardless of whether it serves the constitutional purpose at all,' which is a different and much weaker claim. This is precisely the kind of drift the classification should surface: the coordination function (separation of powers) is real, but it has been extended past its justification into something that functions as a blank check, hence tangled_rope rather than a clean rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_basis_as_abdication_or_restraint,
    'Is rational basis review of copyright term legislation a principled application of separation-of-powers restraint, or a judicial abdication that empties ''limited Times'' of independent meaning?',
    'Compare how courts treat other constitutionally qualified legislative grants (e.g. ''necessary and proper,'' ''reasonable'' searches) where some textual qualifier survives judicial enforcement despite deference; if ''limited Times'' is uniquely unenforceable among comparable clauses, that supports the abdication reading.',
    'If abdication, the judicial_ambiguity_reading should be read as functionally enabling indefinite extension (converging toward the corporate_enclosure_reading in practice); if restraint, the doctrine is a genuine, defensible allocation of institutional competence and this constraint is closer to a rope than a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_basis_as_abdication_or_restraint, conceptual, 'Whether judicial deference on copyright terms is principled restraint or effective abdication of the constitutional ceiling.').

omega_variable(
    committer_kernel_reading_indeterminacy,
    'Given that the constitutional text does not specify a review standard, is the judicial_ambiguity_reading itself a contingent doctrinal choice (Eldred could have gone the other way) or the uniquely correct reading of an institutionally silent clause?',
    'Historical and comparative analysis of how other ''limited Times'' clauses or similarly qualified enumerated powers were treated by courts contemporaneous with the Constitution''s drafting, and examination of the Eldred dissent''s alternative doctrinal framework.',
    'If contingent, this reading is one defensible doctrinal path among several live alternatives (supporting the coexists_with relations to both siblings); if uniquely correct, it forecloses meaningful judicial revival of a searching ''limited Times'' standard, strengthening the corporate_enclosure_reading''s practical dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_indeterminacy, conceptual, 'Whether the deference standard is a contingent doctrinal artifact or a structurally forced reading of an underspecified clause.').

omega_variable(
    public_domain_claimant_standing_problem,
    'Is the diffuse, unorganized nature of the public_domain_claimants victim class a structural feature of the constitutional harm (dispersed future benefit, no concentrated present injury) or an artifact of standing doctrine that could be corrected by different procedural rules?',
    'Examine whether alternative standing frameworks (e.g. organizational standing for libraries/archives, qui tam-style public interest standing) have been proposed or piloted in comparable public-domain-erosion contexts.',
    'If structural, the victim class will remain permanently under-litigated regardless of doctrinal reform elsewhere; if a standing artifact, procedural reform independent of the substantive ''limited Times'' question could rebalance the constraint''s enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_claimant_standing_problem, empirical, 'Whether the victims'' inability to litigate is inherent to the harm structure or a fixable procedural gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement_basis(copy_tr_t1976, observed).
narrative_ontology:measurement(copy_tr_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1986, 0.34).
narrative_ontology:measurement_basis(copy_tr_t1986, observed).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.48).
narrative_ontology:measurement_basis(copy_tr_t1998, observed).
narrative_ontology:measurement(copy_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.58).
narrative_ontology:measurement_basis(copy_tr_t2003, observed).
narrative_ontology:measurement(copy_tr_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2012, 0.55).
narrative_ontology:measurement_basis(copy_tr_t2012, observed).
narrative_ontology:measurement(copy_tr_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2026, 0.55).
narrative_ontology:measurement_basis(copy_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement_basis(copy_be_t1976, observed).
narrative_ontology:measurement(copy_be_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1986, 0.26).
narrative_ontology:measurement_basis(copy_be_t1986, observed).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement_basis(copy_be_t1998, observed).
narrative_ontology:measurement(copy_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement_basis(copy_be_t2003, observed).
narrative_ontology:measurement(copy_be_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement_basis(copy_be_t2012, observed).
narrative_ontology:measurement(copy_be_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement_basis(copy_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.2).
narrative_ontology:measurement_basis(copy_su_t1976, observed).
narrative_ontology:measurement(copy_su_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1986, 0.22).
narrative_ontology:measurement_basis(copy_su_t1986, observed).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.3).
narrative_ontology:measurement_basis(copy_su_t1998, observed).
narrative_ontology:measurement(copy_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.36).
narrative_ontology:measurement_basis(copy_su_t2003, observed).
narrative_ontology:measurement(copy_su_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement_basis(copy_su_t2012, observed).
narrative_ontology:measurement(copy_su_t2026, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2026, 0.35).
narrative_ontology:measurement_basis(copy_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This story is the procedural/doctrinal member of a three-story kernel family on the copyright_constitutional_mandate. public_scaffold_reading and corporate_enclosure_reading disagree about the SUBSTANCE of what 'limited Times' requires (temporary public-good mechanism vs. maximal-property-protection); this story is about WHO decides that substantive question and under what standard of review. Structurally, this reading is upstream of both: the deference standard it establishes is the channel through which the corporate_enclosure_reading's practical outcomes (ever-longer terms) are achieved without ever requiring courts to endorse that reading doctrinally, and it is the mechanism that forecloses the public_scaffold_reading from being judicially enforced even where it might command an intellectual majority among commentators. ε here (0.42) is deliberately lower than what a corporate_enclosure_reading story would likely show, because this constraint's extraction is one level removed — it manufactures the discretion that enables downstream extraction rather than extracting directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
