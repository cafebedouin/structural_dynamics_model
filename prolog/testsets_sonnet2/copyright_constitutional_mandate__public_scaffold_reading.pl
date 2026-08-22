% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Copyright as Temporary Scaffold Toward the Public Domain
 *   domain: intellectual_property/constitutional
 *
 * SUMMARY:
 *   This story instantiates the public-scaffold reading of the copyright
 *   constitutional mandate kernel: copyright is a deliberately time-limited
 *   coordination mechanism whose entire justification is the public domain it
 *   eventually enriches. Under this reading, monopoly is instrumental, not
 *   terminal — the 1790 Act's fourteen-year term, renewable once, exemplifies
 *   the original design; term extensions since (1909, 1976, 1998 Sonny Bono
 *   Act) are read as drift away from, not fulfillment of, the constitutional
 *   bargain. This is emphatically not the same constraint as the
 *   corporate_enclosure_reading (which treats the monopoly as the point) or
 *   the judicial_ambiguity_reading (which treats term length as pure
 *   legislative discretion under rational basis review) — those are separate
 *   constraint files with their own epsilon values, linked here only by
 *   network reference and by the kernel they share.
 *
 * KEY AGENTS:
 *   - congress: agenda-setter under this reading, bound by (not merely enabled by) the progress clause
 *   - public_domain: the beneficiary and intended terminus of the whole arrangement
 *   - downstream_creators and libraries_and_archives: beneficiaries whose interests justify shorter terms and robust fair use
 *   - corporate_rightsholders: excluded from this reading's own justificatory logic despite being powerful political actors
 *   - courts: observers who could enforce the scaffold's limits but frequently defer instead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.28).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.22).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Temporary Scaffold Toward the Public Domain").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '9b12cbf0-f804-4482-bc62-1f74704376a9').
narrative_ontology:cs_kernel_codification('9b12cbf0-f804-4482-bc62-1f74704376a9', fixed_text).
narrative_ontology:cs_authority_grounding('9b12cbf0-f804-4482-bc62-1f74704376a9', lineage).
narrative_ontology:cs_interpretation_layer_present('9b12cbf0-f804-4482-bc62-1f74704376a9').
narrative_ontology:cs_reading_relation('9b12cbf0-f804-4482-bc62-1f74704376a9', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b12cbf0-f804-4482-bc62-1f74704376a9', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('9b12cbf0-f804-4482-bc62-1f74704376a9', foundational, monopoly_is_instrumental_not_terminal).
narrative_ontology:cs_axiom_status(monopoly_is_instrumental_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('9b12cbf0-f804-4482-bc62-1f74704376a9', monopoly_is_instrumental_not_terminal, conventional).
narrative_ontology:cs_axiom('9b12cbf0-f804-4482-bc62-1f74704376a9', foundational, limited_times_is_a_binding_substantive_constraint).
narrative_ontology:cs_axiom_status(limited_times_is_a_binding_substantive_constraint, holdable).
narrative_ontology:cs_axiom_grounding('9b12cbf0-f804-4482-bc62-1f74704376a9', limited_times_is_a_binding_substantive_constraint, conventional).
narrative_ontology:cs_reference_frame('9b12cbf0-f804-4482-bc62-1f74704376a9', founding_era_limited_monopoly_bargain).
narrative_ontology:cs_drift_state('9b12cbf0-f804-4482-bc62-1f74704376a9', post_sonny_bono_extension_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b12cbf0-f804-4482-bc62-1f74704376a9', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, living_authors_during_term).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, progress_of_science_and_useful_arts_clause).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_times_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets copyright term length and scope under the constitutional grant, framing every extension or fair-use carve-out as calibrated to the goal of eventual public-domain enrichment. Under this reading, Congress's discretion is bounded by the 'limited times' and 'progress' language, not open-ended.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Receive a time-limited monopoly on their works, allowing them to capture the market value of creation as an incentive. Under this reading, their protection is a means to future public enrichment, not the end in itself, so extending it past what stimulates further creation is a bug, not a feature.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, living_authors_during_term, beneficiary,
    moderate, biographical, constrained, national).

% The eventual repository of every protected work once terms expire. Under this reading it is the intended terminus of the whole arrangement: copyright is understood as a bridge into free public availability, not a permanent private estate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    powerless, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Future artists, educators, and remixers who will build on works once they enter the public domain or fall under fair use. Under this reading their access is what justifies the temporary monopoly in the first place; expansive fair use and shorter terms serve them directly.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, downstream_creators, beneficiary,
    moderate, generational, constrained, national).

% Preserve and eventually provide free public access to works once protection lapses. Under this reading their preservation mission is a direct expression of the constitutional bargain's terminus, and they press for shorter terms and robust fair-use doctrine to fulfill it sooner.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, libraries_and_archives, beneficiary,
    organized, generational, constrained, national).

% Hold long-lived copyright portfolios and lobby for term extension. Under this reading their interest in indefinite de facto perpetuity is treated as external to the constitutional purpose — a pressure the scaffold reading resists rather than accommodates, so their preferred outcome is not represented in this constraint's own logic.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_rightsholders, excluded,
    powerful, civilizational, arbitrage, global).

% Adjudicate fair use and term challenges. Under this reading, courts are expected to treat 'limited times' and 'promote progress' as substantive constraints capable of being violated, not merely rational-basis-satisfied formalities.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the public-goods problem of creative production: without some temporary exclusivity, creators under-invest in works because they cannot capture returns before free copying disperses the value; the temporary monopoly coordinates incentive-to-create with eventual universal access.
% TRANSFER_FUNCTION: Moves a time-bounded right to exclude from the public at large to the individual creator, for a period justified only by the marginal incentive it produces to create the work in the first place — after which the right dissolves and the work moves to universal free access.
% ABSENT_VOICES: Corporate rightsholders whose actual commercial interest is indefinite de facto control are structurally external to this reading's own logic; the reading treats their extension-lobbying as a distortion of the constitutional bargain rather than a legitimate input, so their voice is present in politics but excluded from this constraint's justificatory structure.
% DISAPPEARANCE_RATIONALE: If the scaffold reading collapsed entirely (i.e., copyright were understood purely as permanent property with no public-domain terminus), the public domain would stop growing, fair-use doctrine would lose its textual anchor, and works would never predictably return to free circulation — libraries, educators, and downstream creators would lose the access on which their long-run plans depend.
% FOUNDING_PROBLEM: Creative and inventive works are non-rivalrous and easily copied; without some incentive mechanism, the market under-produces them relative to social value, but any permanent exclusivity would defeat the ultimate goal of a rich, freely accessible public domain.
% FOUNDING_PROBLEM_CORROBORATION: The constitutional text itself (Article I, Section 8: 'to promote the Progress of Science... by securing for limited Times') is cited by legal historians and public-domain advocacy organizations (outside the direct beneficiary set of current rightsholders) as evidence the founding problem was understood as time-bounded incentive, not perpetual estate; corporate rightsholders and their trade associations dispute this reading, which is precisely the kernel contest this story is one reading of.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored low-to-moderate (0.28 at present) because under this reading's own lights the arrangement is genuine coordination — a real incentive-to-create problem is solved, and the beneficiary set (public domain, downstream creators, libraries) captures real value at term's end. Suppression is low (0.22): no one is coerced into the arrangement, and fair use plus first-sale doctrine leave meaningful room for use during the term. Theater ratio is authored as rising over the interval (0.10 to 0.40) because the SCAFFOLD's DEFINING FEATURE — the sunset — has been legislatively extended repeatedly (1909, 1976, 1998) without a correspondingly increased public-benefit justification, so an increasing share of the arrangement's operation is presented as serving the public-domain goal while its actual sunset keeps receding. This is the reading's own internal critique: a scaffold whose sunset never arrives is drifting toward the corporate_enclosure_reading's structure even while retaining the scaffold's rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here are structurally the public domain, downstream creators, and libraries — low d, since the arrangement is authored (by this reading) to serve them via eventual free access. Living authors during the term are also beneficiaries but at moderate d, since their monopoly is bounded and instrumental rather than an end. Crucially there are NO victims in this reading — the coordination story is not cover for extraction; it is the actual structure, weakened only by term-extension drift, not by an underlying extractive design. This is what most sharply distinguishes this reading from the corporate_enclosure sibling, which would declare corporate_rightsholders as beneficiaries and downstream_creators/public_domain as victims of an effectively permanent enclosure.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification depends entirely on the sunset clause being real and approaching, not merely declared. The rising theater_ratio measurement series is the mechanism by which this reading detects its own potential mandatrophy: if term extensions continue outpacing the incentive rationale, the founding_problem_status shifts from 'contested' toward 'dead' (the incentive problem is solved by far shorter terms; extension serves rent extraction, not creation-incentive), while the disappearance_verdict stays 'world_rearranges' — precisely the status=dead + verdict=world_rearranges mismatch pattern that flags capture. This story authors that tension honestly rather than resolving it, because resolving it would answer the very kernel contest this reading is one voice in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_clause_credibility,
    'Is the copyright term''s ''limited times'' language a genuinely binding sunset, or has repeated legislative extension converted it into an empty formal gesture that never actually arrives?',
    'Track whether any future term extension proposal is defeated on public-domain-enrichment grounds, versus the historical pattern of near-unanimous extension passage (1909, 1976, 1998) with minimal public-domain justification offered.',
    'If sunset never credibly arrives, this reading''s own metrics predict drift toward the corporate_enclosure_reading''s structure — the theater_ratio trend already shows this movement; continued extension would eventually require re-classifying this reading''s own descriptive metrics, not just noting divergence from claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_credibility, empirical, 'Whether the scaffold''s sunset clause remains structurally credible or has become theatrical.').

omega_variable(
    reading_selection_evidence,
    'What in the constitutional text, founding-era history, and subsequent doctrine justifies selecting the public_scaffold_reading over the corporate_enclosure_reading or judicial_ambiguity_reading as the operative frame for THIS story?',
    'This is inherently a conceptual/framing question, not an empirical one: the choice was guided by the founding-era term structure (14 years, renewable once) and the explicit ''promote Progress'' text, which textually orients the clause toward an end-state (public benefit) rather than treating the means (exclusivity) as the end. A sibling generator instantiating corporate_enclosure_reading would cite the same clause''s ''securing... exclusive Right'' language and modern doctrine (e.g., Eldred v. Ashcroft''s rational-basis deference) as its textual anchor instead.',
    'Under the corporate_enclosure_reading, extraction would be authored high and beneficiaries would include corporate_rightsholders with downstream_creators reclassified as victims — a completely different constraint with a different epsilon, not a re-measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence, conceptual, 'Documents the alternative kernel framings and why this reading was selected for this story.').

omega_variable(
    public_domain_as_nonagent_beneficiary,
    'Can a non-agent entity (the public domain itself) meaningfully be modeled as a beneficiary that ''collects'' value, or does this collapse into a proxy for the actual human beneficiaries (downstream creators, libraries, the general public)?',
    'Treat public_domain as agent:false in the stakeholder surface (as authored) and verify the directionality derivation correctly routes its structural benefit through the human beneficiary groups rather than treating it as an independent extraction target or source.',
    'Low — this is primarily a modeling clarity question; mishandling it would not change the classification but could produce confusing per-seat outputs if the engine attempted to assign power/exit dynamics to a non-agent entity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_domain_as_nonagent_beneficiary, conceptual, 'Whether modeling the public domain as a non-agent beneficiary is coherent.').


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
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1976, 0.22).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1790, 0.1).
narrative_ontology:measurement(copy_be_t1909, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1909, 0.15).
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1976, 0.2).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1998, 0.26).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(copyright_constitutional_mandate__public_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__public_scaffold_reading, 0.12).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% Three constraint files share the copyright_constitutional_mandate kernel: this one (public_scaffold_reading, scaffold classification, low-moderate epsilon, no victims), corporate_enclosure_reading (property-maximalist reading, expected tangled_rope or snare classification, higher epsilon, downstream_creators/public_domain as victims), and judicial_ambiguity_reading (procedural-deference reading, likely piton or rope depending on how doctrinal drift is measured). Each has its own epsilon and its own beneficiary/victim structure per the ε-invariance principle; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
