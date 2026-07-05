% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Temporary Scaffold Toward Public Domain Enrichment
 *   domain: intellectual_property/constitutional
 *
 * SUMMARY:
 *   This story instantiates the public_scaffold_reading of the
 *   copyright_constitutional_mandate kernel: copyright's exclusivity is read
 *   as an instrumental, temporary means whose entire justification is that it
 *   eventually and predictably enriches the public domain. On this reading,
 *   'limited times' has real bite, fair use is read generously, and Congress
 *   and the courts are constitutionally obligated to keep the bargain
 *   time-bound. This is deliberately NOT the corporate_enclosure_reading
 *   (which treats copyright as a maximal property right) or the
 *   judicial_ambiguity_reading (which treats term length as pure legislative
 *   discretion under rational-basis deference) — those are separate
 *   constraint files. Under this reading alone, the constraint functions
 *   closer to a scaffold: real coordination function (inducing disclosure),
 *   with a constitutionally-implied sunset built into its own justification.
 *   Rising theater_ratio over the measured interval (0.10 to 0.40) reflects
 *   that as actual terms lengthened (1790's 14+14 years to the 1976 Act's
 *   life+50 to the 1998 Sonny Bono Act's life+70/95-120 years for corporate
 *   works), the public-scaffold justification increasingly became rhetorical
 *   cover for what the sibling corporate_enclosure_reading would describe
 *   candidly as extraction — the rising theater_ratio measures the growing
 *   gap between this reading's professed purpose and observed legislative
 *   practice, without this story becoming that sibling reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.28).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.32).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Scaffold Toward Public Domain Enrichment").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, 'c52fc105-9491-4693-8b9b-1a81ded5567e').
narrative_ontology:cs_kernel_codification('c52fc105-9491-4693-8b9b-1a81ded5567e', fixed_text).
narrative_ontology:cs_authority_grounding('c52fc105-9491-4693-8b9b-1a81ded5567e', lineage).
narrative_ontology:cs_interpretation_layer_present('c52fc105-9491-4693-8b9b-1a81ded5567e').
narrative_ontology:cs_reading_relation('c52fc105-9491-4693-8b9b-1a81ded5567e', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52fc105-9491-4693-8b9b-1a81ded5567e', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('c52fc105-9491-4693-8b9b-1a81ded5567e', foundational, exclusivity_is_instrumental_not_terminal).
narrative_ontology:cs_axiom_status(exclusivity_is_instrumental_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('c52fc105-9491-4693-8b9b-1a81ded5567e', exclusivity_is_instrumental_not_terminal, conventional).
narrative_ontology:cs_axiom('c52fc105-9491-4693-8b9b-1a81ded5567e', foundational, limited_times_requires_genuine_temporariness).
narrative_ontology:cs_axiom_status(limited_times_requires_genuine_temporariness, holdable).
narrative_ontology:cs_axiom_grounding('c52fc105-9491-4693-8b9b-1a81ded5567e', limited_times_requires_genuine_temporariness, conventional).
narrative_ontology:cs_reference_frame('c52fc105-9491-4693-8b9b-1a81ded5567e', progress_clause_public_benefit_bargain).
narrative_ontology:cs_drift_state('c52fc105-9491-4693-8b9b-1a81ded5567e', post_sonny_bono_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c52fc105-9491-4693-8b9b-1a81ded5567e', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, future_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, readers_and_users).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, living_authors_during_term).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_clause_has_teeth).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, copyright_purpose_is_public_benefit_not_property_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a time-limited exclusive right to control and monetize a work as the incentive mechanism the framers intended. Benefits during the term but is understood, under this reading, to hold the right in trust — the term is calibrated to what is needed to induce creation, not to maximize private capture.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, living_authors_during_term, beneficiary,
    moderate, biographical, mobile, national).

% Will draw on the expanding public domain and robust fair use doctrine to build new works. Under this reading, they are the intended long-run beneficiaries of the bargain: shorter terms and generous fair use mean more raw material enters the commons sooner for their use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, future_creators, beneficiary,
    moderate, generational, mobile, national).

% The accumulating body of works whose exclusive rights have expired. This reading treats its steady enrichment as the constitutional point of the whole arrangement — copyright's exclusivity is instrumental to feeding this commons, not an end in itself.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Consume, remix, teach with, and build on copyrighted works. Under a robust fair-use and limited-term regime, their access widens steadily as works enter the commons and as fair use doctrine is read generously against rightsholders.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, readers_and_users, beneficiary,
    powerless, biographical, constrained, national).

% Sets copyright term length and scope pursuant to the constitutional grant to 'promote the progress of science and useful arts.' Under this reading, Congress is constitutionally bound to keep terms genuinely limited and calibrated to the public-benefit purpose, not free to extend terms indefinitely at the request of rightsholder lobbies.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate fair use claims and, on this reading, are expected to police the 'limited times' requirement and the public-benefit purpose actively rather than defer wholesale to legislative extension. Courts applying this reading expand fair use and scrutinize term extensions against the constitutional purpose clause.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, federal_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, federal_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Grants a time-limited exclusive right to authors as an incentive to create and disclose works, in exchange for those works eventually entering an enriched public domain that all future creators and the public can freely use.
% TRANSFER_FUNCTION: Temporarily channels exclusive control and revenue from a work to its author/rightsholder for a bounded term, after which the same work's full value transfers to the public domain — the whole arrangement is a delayed transfer TO the public, not a permanent transfer away from it.
% ABSENT_VOICES: No structural victim group exists under this reading — the bargain's temporariness is what protects downstream users and future creators from being victims. Rightsholder lobbies favoring indefinite extension are present but are not victims of this reading; they are simply not the reading's intended beneficiaries.
% DISAPPEARANCE_RATIONALE: If the constitutional 'limited times' and public-purpose grounding disappeared overnight, nothing would stop legislatures from ratcheting terms toward de facto perpetuity — the public domain would stop growing, fair use doctrine would lose its constitutional anchor, and the incentive-for-disclosure bargain would collapse into unbounded private property. The public domain as a growing commons depends on this reading being live.
% FOUNDING_PROBLEM: Without any time-limited incentive, authors might under-produce and under-disclose creative works (trade-secret hoarding instead of publication); without a mandatory expiration, exclusive rights could ossify into permanent private monopolies over cultural and scientific material that should belong to everyone.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, public-domain advocacy organizations (e.g. Creative Commons, EFF amicus briefs), and dissenting opinions in term-extension litigation (e.g. Justice Breyer's Eldred dissent) attest the founding problem remains live and that the constitutional public-benefit purpose has been substantially unmet as terms have lengthened. This corroboration comes from parties outside the rightsholder-beneficiary set who argue the public-scaffold reading describes the constitutional design but not current practice — which is precisely why this reading competes with the corporate_enclosure_reading rather than having settled the question.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-to-moderate (0.28) because under this reading's own terms, the temporary exclusivity is calibrated to incentive need, not rent maximization — the number describes a coordination mechanism whose cost is bounded by design, even though the historical trend (0.12 to 0.28) shows real drift toward more exclusivity than the founding calibration implied. Suppression is moderate (0.32): fair use, first-sale doctrine, and eventual expiration structurally limit how completely alternatives to licensed use are foreclosed. accessibility_collapse is moderate (0.35) reflecting that during the term, unlicensed use is genuinely restricted, but the restriction is understood as bounded and self-terminating rather than complete foreclosure. Resistance is elevated (0.55) because this reading is actively contested — public domain advocates, library associations, and open-access scholars actively litigate and lobby to hold the scaffold to its stated terms against extension pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim group is declared under this reading: the entire structural point of the public_scaffold_reading is that the arrangement has no losers once the full cycle (grant, then expiration) is honored — living authors get their bounded incentive, and everyone (including those same authors, as members of the public) gets the enriched commons afterward. This is the central structural claim that distinguishes this reading from the corporate_enclosure_reading, where rightsholders benefit and the public domain/future creators are victims of extension. Directionality here places rightsholders and the public on the SAME side of the ledger over the full time horizon, which is precisely the claim other readings dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead or live because the public-scaffold reading's own proponents (courts applying Eldred's dissent logic, public domain scholars) argue the mandate is being actively subverted by term extension even as its formal constitutional grounding remains intact. This is not mandatrophy in the classic sense (function atrophied, form persists) — rather, the reading itself is in live contest with a rival reading (corporate_enclosure) over whether the scaffold's sunset clause is being honored. The rising theater_ratio measurement is the operationalization of that contest: it tracks the growing distance between stated purpose and legislative practice without converting this story into the sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_reading_is_operative_reading,
    'Which of the three kernel readings (public_scaffold, corporate_enclosure, judicial_ambiguity) actually describes how the copyright system operates today, versus which merely describes its founding justification?',
    'Track whether Congress or the courts, when term extension is challenged, are compelled to make an affirmative public-benefit showing (supporting public_scaffold) versus deferring to legislative judgment without such a showing (supporting judicial_ambiguity) versus treating the right as presumptively permanent absent explicit challenge (supporting corporate_enclosure). Eldred v. Ashcroft (2003) is the central data point: the majority opinion tracks closer to judicial_ambiguity, while the dissent (Breyer, Stevens) tracks public_scaffold.',
    'If judicial_ambiguity or corporate_enclosure is the operative reading in practice, the public_scaffold_reading described here is closer to aspirational constitutional theory than an accurate description of the current constraint''s operation — its low epsilon may describe the design intent rather than the current mechanism. This does not change THIS story''s own epsilon (which is authored as the reading''s internal logic), but it bears on whether this reading should be read as descriptively dominant or as one contested claim among three live claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_operative_reading, conceptual, 'Whether the public_scaffold_reading is the operative or merely aspirational reading of the kernel.').

omega_variable(
    term_extension_ratchet_evidence,
    'Does the historical pattern of one-directional term extension (1790: 14+14yr, 1831: 28+14yr, 1909: 28+28yr, 1976: life+50, 1998: life+70) constitute evidence against the public_scaffold_reading''s claim that terms are calibrated to incentive need rather than rentseeking pressure?',
    'Economic analysis of whether marginal incentive to create is plausibly affected by term length beyond an author''s lifetime (it is not, for a living author deciding whether to create); compare lobbying expenditure and timing of extensions to expiration dates of specific high-value copyrights (e.g. the 1998 Act''s proximity to Mickey Mouse''s scheduled entry into the public domain).',
    'If extensions are shown to track rightsholder lobbying and specific expiration deadlines rather than any incentive-calibration logic, this substantially undermines the public_scaffold_reading''s claim that the mechanism remains genuinely temporary-by-design, and supports treating current practice as having drifted toward the corporate_enclosure_reading despite this reading''s continued formal availability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(term_extension_ratchet_evidence, empirical, 'Whether the term-extension pattern is consistent with incentive calibration or with rentseeking capture.').

omega_variable(
    public_domain_as_beneficiary_agency,
    'Can a non-agent entity (the public domain) meaningfully be modeled as a ''beneficiary'' with standing comparable to organized rightsholder interests, given it has no lobbying capacity, no litigation budget, and no organized voice in the legislative process that sets copyright terms?',
    'Compare the resourcing and access of public-domain-advocacy organizations (EFF, Creative Commons, library associations) to that of rightsholder trade groups (RIAA, MPAA, publishers'' associations) in congressional testimony and lobbying expenditure across the relevant extension debates.',
    'If the public domain''s structural beneficiary status is real in constitutional theory but nearly powerless in the actual legislative process that sets terms, that asymmetry is itself evidence for why the corporate_enclosure_reading may better describe practice even where public_scaffold better describes constitutional design — a beneficiary with no seat at the table receives its benefit only when a court or advocate enforces the design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_as_beneficiary_agency, conceptual, 'Whether the public domain''s beneficiary status functions in practice or only in theory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1790, 0.1).
narrative_ontology:measurement(copy_tr_t1909, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1909, 0.15).
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1976, 0.24).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1998, 0.34).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1790, 0.12).
narrative_ontology:measurement(copy_be_t1909, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1909, 0.16).
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1976, 0.22).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1998, 0.26).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(copyright_constitutional_mandate__public_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the copyright_constitutional_mandate kernel. corporate_enclosure_reading treats the same constitutional text as grounding a maximal, near-perpetual property right (beneficiary: rightsholders; victim: public domain/future creators; high epsilon). judicial_ambiguity_reading treats term length as pure legislative discretion under rational-basis review, with courts declining to substantively police the public-benefit purpose (no clear beneficiary/victim structure at the doctrinal level; moderate epsilon reflecting institutional deference rather than either extraction or public benefit). This story (public_scaffold_reading) treats the same clause as mandating genuine temporariness calibrated to public enrichment (beneficiary: public domain and future creators; no victim; low-to-moderate epsilon). The three stories share one constitutional text and one historical record but instantiate structurally distinct constraints because they read the 'limited times... to promote the progress' clause differently — per the epsilon-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
