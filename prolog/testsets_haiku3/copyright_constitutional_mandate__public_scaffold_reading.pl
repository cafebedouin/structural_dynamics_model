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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Constitutional Mandate: Public-Domain Enrichment Reading
 *   domain: intellectual_property/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested Copyright
 *   Clause kernel (Article I, Section 8: 'to promote the Progress of Science
 *   and Useful Arts, by securing for limited Times to Authors and Inventors
 *   the exclusive Right to their respective Writings and Discoveries'). The
 *   public-scaffold reading interprets this mandate as: copyright is a
 *   TEMPORARY mechanism whose purpose is PUBLIC ENRICHMENT. The monopoly is a
 *   means (incentivize creation), not the end (rents are a side effect of the
 *   incentive, not the goal). Under this reading, 'limited times' is a
 *   structural constraint on Congress — a bound, not a discretionary label.
 *   Contrast: the corporate-enclosure reading treats 'limited times' as
 *   naming a legal form (a term exists, however long) while maximizing its
 *   length; the judicial-ambiguity reading defers to Congress's judgment on
 *   what 'limited' means. This story narrates ONLY the public-scaffold
 *   reading. Its beneficiary is the public domain (and the derivative
 *   creators who feed on it). Its epsilon is moderate (0.38 at steady-state)
 *   because copyright IS extractive (it imposes a temporary monopoly), but
 *   the extraction is bounded in time and justified by the incentive it
 *   provides. The claim/metric gap is INTENTIONAL: this reading CLAIMS the
 *   constraint is a scaffold (temporary, justified by the transition) while
 *   the authored metrics show moderate extractiveness and rising theater
 *   (enforcement focus on preventing enclosure has grown). The engine will
 *   measure whether the structural data supports a scaffold classification or
 *   finds a different type.
 *
 * KEY AGENTS:
 *   - public_domain: collective beneficiary (non-agent entity per schema); constitutional end-state
 *   - original_creators: principal agents incentivized by the temporary monopoly
 *   - derivative_creators: depend on public domain and fair use; blocked by long terms
 *   - educational_institutions: custodians of the enriched public domain and fair use
 *   - copyright_holding_corporations: structurally excluded from this reading's mandate; their licensing-extension interests are adverse to public enrichment
 *   - Congress: agenda-setter; sets term length within claimed bounds ('limited times')
 *   - Courts: observers; interpret what 'limited times' means and whether Congress has exceeded it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.38).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.25).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Constitutional Mandate: Public-Domain Enrichment Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property/constitutional_law/political_economy").

narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '50431740-00b8-4539-abe3-2c62ac45dde0').
narrative_ontology:cs_kernel_codification('50431740-00b8-4539-abe3-2c62ac45dde0', fixed_text).
narrative_ontology:cs_authority_grounding('50431740-00b8-4539-abe3-2c62ac45dde0', lineage).
narrative_ontology:cs_interpretation_layer_present('50431740-00b8-4539-abe3-2c62ac45dde0').
narrative_ontology:cs_reading_relation('50431740-00b8-4539-abe3-2c62ac45dde0', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('50431740-00b8-4539-abe3-2c62ac45dde0', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('50431740-00b8-4539-abe3-2c62ac45dde0', foundational, limited_times_is_substantive_bound).
narrative_ontology:cs_axiom_status(limited_times_is_substantive_bound, holdable).
narrative_ontology:cs_axiom_grounding('50431740-00b8-4539-abe3-2c62ac45dde0', limited_times_is_substantive_bound, deontological).
narrative_ontology:cs_axiom('50431740-00b8-4539-abe3-2c62ac45dde0', foundational, copyright_mandates_eventual_public_enrichment).
narrative_ontology:cs_axiom_status(copyright_mandates_eventual_public_enrichment, holdable).
narrative_ontology:cs_axiom_grounding('50431740-00b8-4539-abe3-2c62ac45dde0', copyright_mandates_eventual_public_enrichment, deontological).
narrative_ontology:cs_axiom('50431740-00b8-4539-abe3-2c62ac45dde0', secondary, incentive_rationale_is_primary_justification).
narrative_ontology:cs_axiom_status(incentive_rationale_is_primary_justification, holdable).
narrative_ontology:cs_axiom_grounding('50431740-00b8-4539-abe3-2c62ac45dde0', incentive_rationale_is_primary_justification, empirically_contingent).
narrative_ontology:cs_reference_frame('50431740-00b8-4539-abe3-2c62ac45dde0', temporary_incentive_with_public_enrichment).
narrative_ontology:cs_drift_state('50431740-00b8-4539-abe3-2c62ac45dde0', contemporary_copyright_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('50431740-00b8-4539-abe3-2c62ac45dde0', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, derivative_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, educational_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, original_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The constitutional end-state to which copyright should contribute: works enter the public domain, become freely available for adaptation, study, and reuse by all creators and citizens. This reading treats the public domain as the primary beneficiary of copyright's existence, not a residual or limit case.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Receive a temporary monopoly (limited term) on their work sufficient to incentivize creation and capture initial commercial returns. Under this reading, the term should be calibrated to incentive creation, not to maximize publisher control or estate transfers. Creators benefit from the incentive; they do not benefit from perpetual terms that serve corporate licensing portfolios instead of creator interests.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, original_creators, beneficiary,
    powerful, biographical, mobile, global).

% Depend on entry into the public domain (and fair use) to adapt, remix, and build upon prior works without permission fees or licensing delays. Under this reading, derivative creation is treated as a public good worthy of protection; copyright terms should enable derivative work while monopolies are still in effect through robust fair use doctrine, not foreclose it until decades after the original creator's death.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, derivative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from the public domain and fair use to teach, archive, and study cultural works. This reading treats education and preservation as core functions that copyright duration and fair use doctrine should support, not restrict. Libraries, universities, and public archives are the custodians of the enriched public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, educational_institutions, beneficiary,
    organized, generational, constrained, national).

% Are structurally excluded from this reading's core mandate. This reading does not deny that corporate copyright holders have interests; it treats the corporate interest in indefinite license extension as opposed to the constitutional aim of eventual public enrichment. They would argue for 'limited times' meaning 'as long as commercially useful' and against fair use expansion; their exclusion from this reading is intentional.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_holding_corporations, excluded,
    institutional, generational, trapped, global).

% Sets copyright term length and scope via legislation, constrained by Article I, Section 8 mandate to promote the Progress of Science and Useful Arts by securing exclusive rights for 'limited Times.' Under this reading, Congress's discretion is NOT a blank check; it is bounded by the constitutional end-state (eventual public enrichment) and the instrumental rationale (incentivizing creation, not maximizing extraction).
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, congress, agenda_setter,
    institutional, generational, analytical, national).

% Interpret copyright law and the Constitution. Under this reading, courts should read 'limited Times' as a structural constraint on Congress's delegation power, not as deferentially permitting indefinite extension disguised as a finite term. This reading would support judicial enforcement of the public-enrichment mandate against legislative term-extension that has lost connection to incentive theory.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, courts, observer,
    institutional, generational, analytical, national).

% Depend on the public domain and fair use to access culture, learn from prior works, and build on them. As individual consumers of cultural goods, they benefit from lower prices and free access when works enter the public domain; as potential creators, they benefit from the ability to build on prior culture under fair use. This reading treats citizen access as a constitutional end-value.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, citizens, beneficiary,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, copyright_holding_corporations).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a temporary, structured incentive for creation: an exclusive right to exploit a work commercially for a defined period (calibrated to incentivize creation without suppressing follow-on innovation), with the end-state guarantee that the work will eventually enter the public domain and become freely available to all. This coordinates the interest in incentivizing creative production with the interest in eventual broad cultural access.
% TRANSFER_FUNCTION: Transfers an exclusive-exploitation right from the public to the creator for a limited term; upon expiration, the right reverts to the public. What moves is NOT wealth extraction but temporal sequencing of access: initial scarcity (monopoly period) intended to fund creation, followed by abundance (public domain) to fund derivative work and education. The term-length setting is the core parameter affecting the balance.
% ABSENT_VOICES: Copyright-holding corporations and rent-seeking licensing intermediaries who would argue for perpetual or near-perpetual term extension. They are excluded from this reading's framing because their interest (maximizing the licensing-revenue period) is treated as adverse to the constitutional mandate (eventual public enrichment). Also absent: creators in low-income countries and creators dependent on derivatives, whose fair-use and public-domain interests conflict with Western corporate copyright hegemony. Their absence from legislative negotiation is a structural feature of how copyright law is set.
% DISAPPEARANCE_RATIONALE: If this mandate (copyright as temporary means to public enrichment) disappeared and were replaced by perpetual proprietary control, the cultural commons would contract: works would remain under license-wall indefinitely, derivative creation would be chilled by permission-seeking and licensing costs, educational use would depend on corporate goodwill, and the incentive structure would shift from 'create now to capture exclusive returns' to 'create now and your heirs capture indefinite licensing revenue.' The world of culture, education, and derivative work would reorganize around permission-seeking and licensing rather than free building on the commons.
% FOUNDING_PROBLEM: Incentivizing creative production requires funding creators. Pure gift culture does not fund professional writing, music, or innovation at scale. But unfettered proprietary control discourages derivative work, education, and the cumulative innovation that makes new creation possible. The constitutional mandate solves this: grant a temporary monopoly sufficient to fund creation (incentive), with a guaranteed endpoint (public enrichment) that prevents monopolies from becoming perpetual rent extractions.
% FOUNDING_PROBLEM_CORROBORATION: This reading draws on founding-era documents (Madison's notes on the Copyright Clause, Hamilton's Federalist writings) and the text of the Constitution itself, which explicitly bounds copyright term to 'limited Times.' Contemporary evidence supporting this reading: (1) economic studies showing copyright's incentive effect plateaus and even reverses at very long terms (Boldrin & Levine, Bloom); (2) legislative testimony from creators (authors, musicians, software developers) distinguishing between incentive-adequate terms and rent-extension; (3) judicial opinions (notably Judge Richard Posner and Justice Stephen Breyer's concurrences on term extension) arguing for bounded interpretation; (4) international comparison showing jurisdictions with shorter terms (Europe's 50-70 year terms vs. US 95-year terms) have robust creative production. The corporate-enclosure reading cites similar foundational texts but reads them as protecting property rights maximally. The empirical corroboration for bounded-term incentivizing comes from outside both the corporate beneficiary set and the pure-public-domain advocacy set.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.38) is moderate and stable because copyright DOES impose a monopoly (extraction), but it is time-bounded (not snare-like). The temporal sequencing is the structure: extraction now, reversion later. Suppression is low (0.25) because the reading does not rely on censoring alternatives; instead, it relies on fair use and public-domain entry to preserve derivative creation pathways. Theater rises modestly (0.12 to 0.22) and then plateaus: as term extension has accumulated (de facto approaching perpetuity despite de jure 'limited times'), enforcement has increasingly focused on preventing public-domain entry and fair-use expansion (the DMCA, licensing infrastructure) — the performative enforcement of 'limitation' while the actual limit erodes. Accessibility collapse is low-moderate (0.42): alternatives (derivative work, fair use, open licensing) remain available but are constrained by the expanding monopoly period. Resistance is HIGH (0.71) because this reading faces organized opposition from corporate copyright holders (a powerful seat) and generates ongoing judicial/legislative contestation. The measurement grid spans 50 time units (roughly representing decades). The extractiveness rise 0-24 models the period of increasing copyright term (Sonny Bono Act, Digital Millennium Copyright Act); the plateau 24-50 models the de facto perpetual term despite de jure 'limited times'. Theater's plateau suggests the reading's enforcement infrastructure has stabilized but not deepened (the theatrical component saturated as copyright became effectively perpetual).
 *
 * PERSPECTIVAL GAP:
 *   The corporate-enclosure reading and this public-scaffold reading should compute very differently from the same factual base. From the corporate seat, copyright is a property right to be maximized; 'limited times' is a legal form, not a structural bound; the constraint type is rope (coordination of creator incentives) or snare (monopoly extraction is the goal). From the public-scaffold seat, copyright is temporary and bounded; enforcement focus on preventing enclosure is the signal of drift; the constraint type is scaffold (means justified by the transition) or piton (means no longer justified, end atrophied, persisting by inertia). The engine should compute seat-divergent types from the structural data. What makes this interesting is that BOTH readings cite the SAME constitutional text and the SAME historical record. The divergence is in how they weigh the textual bounds ('limited times') and the stated purpose ('progress') against corporate licensing precedent and legislative expansion. This reading treats the text and purpose as primary; it reads the legislative expansion as attempted override, not legitimate interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Original creators and the public domain are the structural beneficiaries in this reading: they receive the incentive (temporary monopoly) and the eventual enrichment (public domain). Derivative creators and educational institutions benefit downstream. Copyright-holding corporations are excluded because their interest in term extension is read as adverse to the constitutional mandate. The directionality computation should show: (1) original creators near symmetric or slight-beneficiary d (they get incentive; they do NOT capture the rent-extension-to-licensees effect); (2) derivative creators and citizens with high d (they bear the cost of the extended monopoly); (3) corporations with high d in the corporate-enclosure reading, but EXCLUDED from this reading's framing. Congress sits as agenda-setter but analytically: it has the power to set terms, but this reading treats that power as bounded by the constitutional mandate, not unfettered.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading potentially exhibits mandatrophy: the founding problem (lack of incentive for creative production) has been solved — professional writing, music, software development are robust industries. Copyright term is now vastly longer than the empirical evidence suggests is necessary to incentivize creation (Boldrin & Levine find diminishing returns above 20-30 years; current US terms are 95+ years for corporate works). Yet the constraint persists and has GROWN in enforcement intensity (DMCA, licensing expansions, fair-use restrictions). This gap between founding problem status (DEAD — incentives exist at shorter terms) and constraint persistence (ALIVE and expanding) is the hallmark of mandatrophy. The theater ratio captures this: enforcement focused on preventing public-domain entry and fair use (performative maintenance of 'limited times' while the limit erodes) rather than on incentivizing creation (the original function). A mandatrophy reading would reclassify this from scaffold (legitimate temporary measure) to snare (disguised extraction) once the founding problem dies and enforcement shifts from incentive-maintenance to enclosure-defense. This story authors a scaffold reading; the mandatrophy analysis documents the risk that the classification could be contested. The six_questions.founding_problem_status field records this as 'contested' — the reading asserts the founding problem is dead (incentives exist at shorter terms), corporate interests assert it is live (longer terms further incentivize). The mismatch consumer will flag this as a mandatrophy candidate for downstream investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bounded_term_interpretation,
    'Does ''limited Times'' in the Copyright Clause impose a structural bound on Congress''s delegation power (limiting how long a ''limited time'' can be), or is it merely a formal requirement that a term exists, permitting indefinite practical extension?',
    'Judicial decision holding that perpetual copyright violates the Constitution, or Supreme Court decision interpreting ''limited times'' as a substantive limit rather than a formal category. Alternatively, empirical evidence showing the original framers'' understanding of ''limited'' (historical sources, founding-era patent and copyright usage) and whether they intended a bound on term length or merely that perpetuity was unavailable as a legal form.',
    'If ''limited times'' is substantive and bounds Congress, this reading''s classification as a scaffold (temporary, justified by the transition) holds — enforcement focus on enclosure-prevention is justified. If ''limited times'' is merely formal and permits practical perpetuity, this reading collapses into the corporate-enclosure reading, and the classification becomes snare (indefinite monopoly extraction under a label of ''limitation'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bounded_term_interpretation, conceptual, 'The core framing ambiguity between bounded-term and formal-limitation interpretations of ''limited Times.''').

omega_variable(
    copyright_incentive_plateau,
    'At what copyright term length does the incentive effect for creative production plateau or reverse? Does empirical evidence support that current US term lengths (95+ years for corporate works) continue to incentivize creation, or have they become rent-extraction unmoored from incentive theory?',
    'Systematic empirical study comparing creative output (books published, music released, software developed) in jurisdictions with different copyright terms, controlling for other factors. Analysis of creator surveys: do professional creators cite 95-year terms as necessary to incentivize their work, or do they cite shorter terms (5-20 years) as sufficient for recovery and incentive? Historical comparison: were creative industries robust in eras with shorter terms (pre-1976 US 28+28 year terms)?',
    'If empirical evidence shows the incentive plateaus at 30-50 years and current terms add no incentive at the margin, the founding problem (lack of incentive) is DEAD — the constraint persists due to inertia and corporate rent-capture, not to justify creation. This would transform the classification from scaffold (justified temporary measure) to piton (atrophied function, performative maintenance). If evidence shows incentive continues at higher terms, this reading''s bounded-term proposal would be undersized, and the classification becomes contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_incentive_plateau, empirical, 'Whether the empirical incentive effect of copyright still exists at current US term lengths or has exhausted.').

omega_variable(
    corporate_enclosure_tradeoff,
    'As copyright terms extend toward perpetuity, does derivative creative production (sequels, remixes, adaptations, retellings, fan works) increase, stay constant, or decrease? Is the extended monopoly period justified by the incentive it provides to original creation, or does it suppresses derivative creation that would generate additional cultural value?',
    'Comparative analysis of derivative works produced in eras/jurisdictions with shorter copyright terms vs. longer terms. Data on fair-use litigation trends and licensing-negotiation costs: are derivative creators increasingly deterred by licensing barriers? Evidence from creative communities (fan fiction, remix culture, literature adaptation, software forks) on whether shorter or longer copyright terms correlate with more derivative work and cultural remix.',
    'If derivative creation is suppressed by extended terms, the net cultural benefit of the term extension is negative — it trades initial-creation incentive for follow-on-creation suppression. This reading treats derivative creation as a public good; if the evidence shows it is suppressed, this reading''s mandate is being violated in practice, and the constraint drifts from scaffold (justified means) to snare (net-extraction structure). If derivative creation is unaffected by term length, the tradeoff is moot and the term-extension question becomes purely about distributional fairness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_enclosure_tradeoff, empirical, 'Whether extended copyright terms suppress derivative creation and cultural remix.').

omega_variable(
    reading_alignment_with_foundational_intent,
    'Does this reading''s interpretation of ''limited Times'' and ''Progress of Science'' align with the original framers'' intent, or is it a retrospective rationalization that conflicts with how the Framers understood copyright''s scope and term?',
    'Historical scholarship on the Constitutional Convention debates, Federalist Papers, and founding-era copyright practice (state constitutions, early federal legislation). Cross-reference with Madison''s writings on the Copyright Clause and Hamilton''s views on incentivizing production. Comparison with contemporary patent-term debates, which paralleled copyright debates and may illuminate the ''limited times'' intent.',
    'If the Framers intended perpetual or near-perpetual terms (treating ''limited'' as merely excluding perpetuity per se), this reading''s bounded-term proposal is historically unfounded — it becomes an aspirational reframing rather than recovery of original intent. If the Framers intended substantive bounds (20-50 years, estimated based on founding-era precedent), this reading recovers original intent and gains legitimacy. If the evidence is ambiguous, the reading becomes more vulnerable to the corporate-enclosure reading''s claim to historical warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alignment_with_foundational_intent, empirical, 'Whether this reading aligns with the Framers'' original intent regarding copyright scope and term length.').

omega_variable(
    fair_use_doctrine_viability,
    'Can fair use and public-domain entry serve as adequate safeguards for derivative creation, education, and innovation, or is the current fair-use doctrine so eroded (DMCA, licensing precedent, orphan-work barriers) that longer copyright terms systematically block the derivative pathways this reading relies on?',
    'Analysis of fair-use litigation outcomes and licensing-negotiation barriers: what percentage of derivative-creation projects obtain fair-use permission vs. licensing rights vs. abandonment? Evidence from libraries, educators, and derivative creators on accessibility of orphan works and fair-use boundaries. Comparison of fair-use viability in high-copyright-term regimes (US 95 years) vs. lower-term regimes (EU 50-70 years) or open-licensing jurisdictions.',
    'If fair use is viable and public-domain entry accessible, shorter copyright terms combined with robust fair-use doctrine achieve this reading''s mandate efficiently. If fair use is eroded and public-domain entry blocked (by term extension, DMCA anti-circumvention, licensing monopolies), the reading''s structural protection for derivative creation is illusory — the constraint persists as extraction without the derivative pathways. This would indicate mandatrophy or drift toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_doctrine_viability, empirical, 'Whether fair-use doctrine and public-domain accessibility remain viable under current copyright regime or have eroded to illusory status.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the public-scaffold reading''s interpretation of ''limited times'' as a bounded mandate logically foreclose the corporate-enclosure reading''s interpretation of ''limited times'' as naming a legal form permitting indefinite extension? Or do both readings coexist as live interpretive options within the same constitutional text?',
    'Formal analysis of the textual evidence and the logical structure of the readings'' claims. Does the text ''limited Times'' admit of a reading that is compatible with perpetual extension, or does the text itself rule it out? Are there substantive principles (rule of law, constitutional limits on delegation, textual bounds) that force a choice between readings, or do they represent genuinely incommensurable framings?',
    'If the readings are mutually foreclosing (the text compels one over the other), the corporate-enclosure reading is ruled out by this reading, and the constraint-family analysis identifies a genuine foreclosure relation. If the readings coexist (both are textually defensible but incompatible policy choices), the relation is ''coexists_with'' and both remain live options for different parties/jurisdictions. The answer determines how the engine models the kernel family''s dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the public-scaffold and corporate-enclosure readings are logically foreclosing or coexisting within the Copyright Clause kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyright_public_scaffold_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(copyright_public_scaffold_tr_t8, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(copyright_public_scaffold_tr_t16, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(copyright_public_scaffold_tr_t24, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(copyright_public_scaffold_tr_t32, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement(copyright_public_scaffold_tr_t40, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(copyright_public_scaffold_tr_t50, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(copyright_public_scaffold_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(copyright_public_scaffold_be_t8, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(copyright_public_scaffold_be_t16, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(copyright_public_scaffold_be_t24, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(copyright_public_scaffold_be_t32, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(copyright_public_scaffold_be_t40, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(copyright_public_scaffold_be_t50, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(copyright_public_scaffold_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(copyright_public_scaffold_su_t8, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(copyright_public_scaffold_su_t16, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(copyright_public_scaffold_su_t24, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(copyright_public_scaffold_su_t32, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 32, 0.25).
narrative_ontology:measurement(copyright_public_scaffold_su_t40, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(copyright_public_scaffold_su_t50, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__public_scaffold_reading, 0.2).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Copyright Clause kernel (copyright_constitutional_mandate). Three live readings decompose the kernel along different axes: (1) public_scaffold_reading (this story): copyright is temporary, aimed at public enrichment; enforces that 'limited times' is substantive bound. (2) corporate_enclosure_reading: copyright is a maximized property right; 'limited times' is formal requirement. (3) judicial_ambiguity_reading: 'limited times' is a zone of legislative discretion; courts defer. The readings share the same referent (the Copyright Clause and its operation) but have incommensurable ε values and beneficiary structures because they read the same text as bearing different obligations. They are linked via network.affects_constraints in both directions: public_scaffold influences corporate_enclosure (corporate reading must defend term extension against the text's 'limited times' language) and influences judicial_ambiguity (the extent of 'limited times' bound is substantive, not merely formal). This story narrates only the public-scaffold reading per Rule 1 (generate one reading as clean ε-invariant constraint). The sibling readings are authored as separate constraint stories with their own omegas, measurements, and cs_structure fields.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__public_scaffold_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
