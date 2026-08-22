% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Temporary Public-Good Scaffold (Public Scaffold Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the public_scaffold_reading of the
 *   copyright_constitutional_mandate kernel: copyright is read as a
 *   deliberately temporary, instrumentally-bounded monopoly whose entire
 *   justification is the eventual enrichment of the public domain, not the
 *   protection of an author's property interest as an end in itself. Under
 *   this reading the 1790 Act's 14-year initial term (renewable once)
 *   represents the baseline design intent; the term extensions of 1909, 1976,
 *   and especially 1998 (Sonny Bono Copyright Term Extension Act) are read,
 *   from within this framework, as drift AWAY from the founding scaffold
 *   logic, not as legitimate exercises of the same purpose — but this
 *   reading's own metrics still register only low-to-moderate extraction
 *   because the doctrinal apparatus of fair use, the eventual (if delayed)
 *   reversion to public domain, and the absence of any coercive victim group
 *   keep the arrangement closer to Rope/Scaffold than to Snare. This is a
 *   genuinely different constraint from the corporate_enclosure_reading
 *   (which reads the same clause as maximal-protection property right) and
 *   the judicial_ambiguity_reading (which reads term-setting as unreviewable
 *   legislative discretion) — each is authored as its own sibling story with
 *   its own epsilon, per the ε-invariance principle. Under the
 *   public-scaffold reading there is no identified victim class: authors who
 *   forgo eventual reversion are understood as having received their
 *   bargained-for incentive already, not as being extracted from.
 *
 * KEY AGENTS:
 *   - public_domain: ultimate beneficiary (analytical/analytical) — the entire justification for the arrangement, has no agency of its own but is what everything is instrumentally organized around
 *   - authors_and_creators: incentivized party (moderate/constrained) — receive the temporary monopoly as inducement to create and disclose
 *   - downstream_creators: secondary beneficiary (moderate/constrained) — gain raw material each time a term expires
 *   - congress: agenda_setter (institutional/arbitrage) — sets term length and fair use scope, bound (on this reading) by the constitutional purpose clause
 *   - courts: observer with live substantive role (institutional/analytical) — tests legislative action against the promote-progress standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.28).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.32).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Public-Good Scaffold (Public Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, 'bd6847be-5a53-4699-b718-62cfaa2e44dc').
narrative_ontology:cs_kernel_codification('bd6847be-5a53-4699-b718-62cfaa2e44dc', fixed_text).
narrative_ontology:cs_authority_grounding('bd6847be-5a53-4699-b718-62cfaa2e44dc', lineage).
narrative_ontology:cs_interpretation_layer_present('bd6847be-5a53-4699-b718-62cfaa2e44dc').
narrative_ontology:cs_reading_relation('bd6847be-5a53-4699-b718-62cfaa2e44dc', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd6847be-5a53-4699-b718-62cfaa2e44dc', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('bd6847be-5a53-4699-b718-62cfaa2e44dc', foundational, copyright_is_instrumental_not_natural_right).
narrative_ontology:cs_axiom_status(copyright_is_instrumental_not_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('bd6847be-5a53-4699-b718-62cfaa2e44dc', copyright_is_instrumental_not_natural_right, conventional).
narrative_ontology:cs_axiom('bd6847be-5a53-4699-b718-62cfaa2e44dc', foundational, limited_times_clause_has_operative_meaning).
narrative_ontology:cs_axiom_status(limited_times_clause_has_operative_meaning, holdable).
narrative_ontology:cs_axiom_grounding('bd6847be-5a53-4699-b718-62cfaa2e44dc', limited_times_clause_has_operative_meaning, conventional).
narrative_ontology:cs_reference_frame('bd6847be-5a53-4699-b718-62cfaa2e44dc', founding_era_instrumentalist_bargain).
narrative_ontology:cs_drift_state('bd6847be-5a53-4699-b718-62cfaa2e44dc', post_sonny_bono_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd6847be-5a53-4699-b718-62cfaa2e44dc', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, educators_and_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_clause_has_operative_meaning).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, copyright_is_instrumental_not_natural_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulating stock of works no longer under copyright, available for anyone to use, adapt, and build upon without permission or payment. Under this reading, the public domain is the entire point of the arrangement — copyright's temporary monopoly exists solely to eventually feed this pool. It has no agency of its own; it grows or stagnates depending on whether terms actually expire.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Receive a time-limited exclusive right as the incentive mechanism that induces creation in the first place. Under the public-scaffold reading, this incentive is calibrated to be just enough to induce production, not a perpetual estate — the author's interest is understood as instrumental to the public-good end, not as an end in itself.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators, agenda_setter).

% Adapters, remixers, scholars, and follow-on artists who need access to prior works after protection lapses in order to build new works. They benefit directly and immediately whenever a term actually sunsets — every expiration event enlarges their raw material.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators, beneficiary,
    moderate, generational, constrained, national).

% Sets term lengths, fair use contours, and enforcement mechanisms under the constitutional grant. Under this reading, Congress is bound by the Copyright Clause's stated purpose — 'to promote the Progress of Science and useful Arts' — meaning term-setting is not open-ended discretion but instrumentally constrained legislative action in service of the public-domain end.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).

% Preserve and provide access to works; their mission is structurally aligned with an expanding, predictably-growing public domain. Fair use expansion and orphan-works provisions under this reading are read as fulfilling copyright's mandate, not as exceptions grudgingly carved from an owner's property right.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives, beneficiary,
    organized, generational, constrained, national).

% Use copyrighted and public-domain works for teaching and scholarship. Broad fair use and a reliably growing public domain reduce their transaction costs and permission-seeking burden; the public-scaffold reading treats their access needs as evidence of the arrangement functioning as designed.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, educators_and_researchers, beneficiary,
    moderate, generational, constrained, national).

% Adjudicate whether specific term extensions or enforcement mechanisms remain consistent with the constitutional purpose. Under the public-scaffold reading, courts are understood as having a live substantive role — testing legislative action against the 'promote progress' purpose — rather than deferring wholesale to legislative judgment.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grants a time-limited exclusive right to authors as an incentive to create and disclose, in exchange for the guaranteed eventual enrichment of a shared public domain that anyone can build upon without payment or permission — solving the public-goods underproduction problem inherent in freely copyable creative and inventive work.
% TRANSFER_FUNCTION: Temporarily moves exclusive-use rights from the public to the author (as the incentive), then moves the completed, disclosed work permanently from private control into the public domain when the term expires. The net long-run transfer, under this reading, runs FROM temporary private control TO permanent public benefit — the monopoly interval is the cost of getting the transfer to happen at all.
% ABSENT_VOICES: Rightsholder trade associations who favor maximal duration and interpretive readings that treat copyright as closer to a natural property right are not centered in this reading's frame; their view is the corporate_enclosure_reading, addressed as a separate, sibling constraint rather than folded in here.
% DISAPPEARANCE_RATIONALE: If copyright disappeared overnight under this reading's own terms, the intended incentive-to-create mechanism would vanish immediately, but so would the constraint's own justification for holding anything back from the public domain — every existing work would become public domain instantly, which is the direction this reading treats as the ultimate good, just arrived at without the transitional incentive period. The world does rearrange: publishing economics, advance payments, and disclosure incentives for not-yet-created works would need a substitute mechanism.
% FOUNDING_PROBLEM: Absent legal protection, authors and inventors have weak incentive to invest in creating and publicly disclosing works, because free riders can copy immediately; the founding problem was ensuring works get created AND eventually released into the shared public stock, rather than kept as trade secret or under-produced altogether.
% FOUNDING_PROBLEM_CORROBORATION: The Constitutional Convention's recorded debate and the Copyright Clause's own text ('to promote the Progress of Science') are cited by legal historians and public-domain advocacy organizations (e.g., Public Knowledge, Creative Commons, and numerous law-review historians) as corroborating that the founding purpose was public enrichment via eventual expiration, not an author property entitlement. This corroboration comes from outside the rightsholder industries that benefit from long terms — the same historical record is disputed by the corporate_enclosure_reading, which reads the founders' intent differently.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at 2024) and suppression moderate (0.32) because this reading treats the arrangement as fundamentally in service of the public domain, with real friction (enforcement, fair-use litigation costs, term extensions that delay reversion) but no identified extraction victim — the temporary monopoly is compensation for value already delivered (creation and disclosure), not a toll extracted from a trapped party. Theater ratio rises modestly (0.05 to 0.22) reflecting increasing gap between the stated public-domain-enrichment purpose and the observable slowing of actual reversion events after 1976 and especially after 1998, but stays well below the level that would indicate the coordination function has been hollowed out — under this reading the scaffold still functions, just less promptly than the founding design intended. Accessibility collapse is authored moderate (0.35): fair use and the eventual guarantee of reversion mean alternatives to seeking permission are not fully foreclosed. Resistance is authored moderately high (0.55) because public-domain advocates, librarians, and open-access movements actively contest term extensions precisely because they read those extensions as departures from this reading's own logic.
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared under this reading because its structural claim is that the exclusive right authors hold is fair exchange for future public enrichment, not extraction from any party. The public domain, downstream creators, libraries, and educators are declared beneficiaries because the entire arrangement is instrumentally organized around eventually delivering value to them. Authors are also beneficiaries (of the incentive) rather than payers, because on this reading they receive exactly what the bargain promises. This is the key structural distinction from the corporate_enclosure_reading, which would treat authors/rightsholders as the primary beneficiary and the public as bearing an extraction cost from over-long terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (inducing creation and disclosure that would otherwise be underproduced) is authored as contested rather than dead: on this reading's own terms the problem remains live (creative and inventive underproduction absent incentive is a persistent economic fact), but the SPECIFIC mechanism — timely reversion to the public domain — has been intermittently deferred by term extensions that this reading treats as drift from, not fulfillment of, the founding design. This is precisely the divergence the classification is built to surface: a scaffold claim whose sunset clause exists in law but has been repeatedly pushed back is not yet a snare (no victim, no coercive extraction) but is trending toward piton territory if reversion continues to be indefinitely deferred — which is why declaring has_sunset_clause: true here is a claim about original design, not a guarantee that the sunset is honored on schedule.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    term_extension_as_drift_or_fulfillment,
    'Do repeated term extensions (1831, 1909, 1976, 1998) represent a betrayal of the public-scaffold design, or a legitimate recalibration of the incentive within the same instrumental framework?',
    'Compare authored legislative history and economic rationale offered for each extension against the counterfactual creation/disclosure rate absent extension; if extensions were substantially driven by rightsholder lobbying disconnected from incentive economics (as the Sonny Bono Act''s retroactive application to already-created works suggests), that supports the drift reading over the fulfillment reading.',
    'If extensions are drift, the public-scaffold reading''s own metrics should be trending toward tangled_rope or piton as the coordination function decays into inertial protection; if fulfillment, the scaffold classification remains stable indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_extension_as_drift_or_fulfillment, conceptual, 'Whether term extensions are internal recalibration or drift away from the public-scaffold design.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Copyright Clause''s text and drafting history genuinely more consistent with the public-scaffold reading than with the corporate_enclosure_reading or judicial_ambiguity_reading, or is the constitutional text itself irreducibly ambiguous between all three?',
    'Original-meaning historical analysis of the 1787 Convention debates, the 1790 Act''s actual term structure, and comparative analysis with contemporaneous English and state copyright statutes (e.g., the Statute of Anne''s explicit public-benefit framing) versus subsequent doctrinal drift in case law (e.g., Eldred v. Ashcroft''s rational-basis deference).',
    'If the text is genuinely indeterminate among the three readings, no single reading can claim exclusive constitutional fidelity, and the kernel remains permanently contested rather than resolvable by better historical scholarship — reinforcing the need to treat these as three coexisting constraint stories rather than seeking a single ''correct'' epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the constitutional kernel is genuinely resolvable toward one reading or irreducibly multi-valent.').

omega_variable(
    reversion_measurement_ambiguity,
    'Is the appropriate metric for ''is the scaffold functioning'' the STOCK of works currently in the public domain, or the RATE of new reversions per year relative to the rate of new copyrightable works created?',
    'Track annual reversion counts (works whose term expires) against annual new-registration counts over the measured interval; a declining reversion-to-creation ratio would indicate the scaffold is falling behind its own design target even while the absolute public-domain stock still grows.',
    'Using stock alone would understate the drift the term extensions represent, since stock only ever grows (or is flat during moratoria) and never shows the deceleration; using rate would more sharply reveal the 1998 extension''s effect of freezing an entire generation of reversions for 20 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversion_measurement_ambiguity, empirical, 'Whether public-domain stock or reversion rate is the correct operationalization of scaffold health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement_basis(copy_tr_t1790, observed).
narrative_ontology:measurement(copy_tr_t1909, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1909, 0.08).
narrative_ontology:measurement_basis(copy_tr_t1909, observed).
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1976, 0.14).
narrative_ontology:measurement_basis(copy_tr_t1976, observed).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement_basis(copy_tr_t1998, observed).
narrative_ontology:measurement(copy_tr_t2012, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2012, 0.21).
narrative_ontology:measurement_basis(copy_tr_t2012, observed).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(copy_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1790, 0.08).
narrative_ontology:measurement_basis(copy_be_t1790, observed).
narrative_ontology:measurement(copy_be_t1909, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1909, 0.12).
narrative_ontology:measurement_basis(copy_be_t1909, observed).
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1976, 0.18).
narrative_ontology:measurement_basis(copy_be_t1976, observed).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1998, 0.24).
narrative_ontology:measurement_basis(copy_be_t1998, observed).
narrative_ontology:measurement(copy_be_t2012, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2012, 0.26).
narrative_ontology:measurement_basis(copy_be_t2012, observed).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(copy_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1790, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1790, 0.1).
narrative_ontology:measurement_basis(copy_su_t1790, observed).
narrative_ontology:measurement(copy_su_t1909, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1909, 0.14).
narrative_ontology:measurement_basis(copy_su_t1909, observed).
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1976, 0.2).
narrative_ontology:measurement_basis(copy_su_t1976, observed).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1998, 0.28).
narrative_ontology:measurement_basis(copy_su_t1998, observed).
narrative_ontology:measurement(copy_su_t2012, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement_basis(copy_su_t2012, observed).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2024, 0.32).
narrative_ontology:measurement_basis(copy_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__public_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the copyright_constitutional_mandate kernel, decomposed per the ε-invariance principle rather than authored as a single observer-relative constraint. corporate_enclosure_reading authors a substantially higher epsilon and names downstream users/public domain as victims of enclosure; judicial_ambiguity_reading authors a lower suppression/resistance profile grounded in procedural deference rather than substantive purpose-review. All three share the same underlying constitutional text (the Copyright Clause) but diverge in beneficiary/victim structure, claimed type, and epsilon because each reading treats a structurally different claim as the operative one — this is the BGS-pattern decomposition, not a single constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
