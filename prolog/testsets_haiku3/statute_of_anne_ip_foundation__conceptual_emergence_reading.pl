% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) inaugurated a new way of thinking about
 *   intellectual property: as a limited-term regulatory incentive for authors
 *   and learning, rather than a perpetual monopoly granted to printers or
 *   publishers. This reading focuses on the conceptual innovation itself—the
 *   statute created a new legal category and a new intellectual space.
 *   Copyright 'became thinkable' as a distinct category (limited, regulatory,
 *   author-centered) that had no prior natural analogue. The statute did not
 *   merely allocate existing rights; it instantiated a novel concept. The
 *   beneficiary is the public-learning function made explicit by the
 *   statute's framing; the victim is the perpetual-monopoly frame that
 *   previously governed printing. This reading is one of three
 *   interpretations of the statute's kernel (the text, the legislative
 *   intent, the subsequent legal tradition). The claim (rope: genuine
 *   coordination) and the metrics (moderate extractiveness, low suppression)
 *   reflect this reading's frame: the statute solves a real coordination
 *   problem (incentive + learning) with minimal coercive overhead, sustained
 *   by the conceptual frame itself rather than by heavy enforcement.
 *
 * KEY AGENTS:
 *   - Parliament: legislative agenda-setter; authored the statute and its explicit conceptual frame
 *   - Public learning: beneficiary of the limited-term structure; unorganized but made explicit in the statute's logic
 *   - Authors: beneficiary of limited-term monopoly; displaced the Stationers' Company as the primary IP beneficiary
 *   - Stationers' Company: victim of the conceptual displacement; loses authority to define perpetual monopoly
 *   - Readers and future creators: ultimate beneficiaries when works enter public domain; absent from deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.38).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.22).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3').
narrative_ontology:cs_kernel_codification('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', fixed_text).
narrative_ontology:cs_authority_grounding('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', lineage).
narrative_ontology:cs_interpretation_layer_present('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3').
narrative_ontology:cs_reading_relation('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', statute_of_anne_ip_foundation__institutional_reallocation_reading, influences).
narrative_ontology:cs_reading_relation('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', foundational, copyright_is_limited_regulatory_tool).
narrative_ontology:cs_axiom_status(copyright_is_limited_regulatory_tool, holdable).
narrative_ontology:cs_axiom_grounding('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', copyright_is_limited_regulatory_tool, conventional).
narrative_ontology:cs_axiom('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', foundational, public_learning_is_primary_beneficiary).
narrative_ontology:cs_axiom_status(public_learning_is_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', public_learning_is_primary_beneficiary, instrumental).
narrative_ontology:cs_reference_frame('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', limited_regulatory_copyright).
narrative_ontology:cs_drift_state('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', donaldson_v_beckett_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('16f5969f-e5ef-44b1-9cfd-6a3d2807d0a3', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors_as_incentive_subjects).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, readers_future_creators).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acts as the legislative body that authored and passed the statute. Framed the innovation as creating a limited-term monopoly to incentivize authors and learning, explicitly rejecting perpetual monopoly as the alternative. Sets the conceptual frame that IP is a regulatory tool, not property.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Gains from works entering the public domain after the limited term expires. The statute's conceptual innovation is that learning is not a residual benefit but a primary goal of the regulatory structure. Public learning is an abstraction—the indirect beneficiary of all readers, scholars, and creators who build on prior work.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning, beneficiary,
    powerless, civilizational, analytical, national).

% Receive a limited-term monopoly (14 years, renewable once) as incentive to create and publish. The statute treats authors as the proper beneficiaries of copyright, displacing the Stationers' Company from that role. Authors gain monopoly rents for a bounded time, but the statute limits their power to exclude after expiration. They are also future payers when others' copyrights restrict them.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, payer).

% Previously held perpetual monopoly over printing and publishing in England via royal charter. The statute's conceptual frame—IP as limited regulatory tool rather than property—displaces their claim to perpetual control. They lose the authority to set the conceptual boundary of what copyright is. Their exit is minimal: they must either accept the limited-term frame or litigate against Parliament.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    powerful, biographical, constrained, national).

% Benefit from works entering public domain, enabling future creation and remixing without payment or permission after copyright expires. They are unorganized and non-vocal during the statute's passage, but the statute's conceptual frame makes them explicit beneficiaries of the design—learning is the stated goal.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, readers_future_creators, beneficiary,
    powerless, civilizational, analytical, national).

% Tasked with interpreting and applying the new statutory concept of limited-term copyright. They must adjudicate disputes by reference to the statute's stated conceptual frame (incentivizing authors and learning), not perpetual property logic. The statute creates a new interpretive vocabulary they must work within.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, judges_legal_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a common legal frame for thinking about IP: copyright is explicitly positioned as a regulatory tool with stated beneficiaries (authors, learning) and a sunset mechanism (limited term), not as an absolute property right. This solves the coordination problem of how to incentivize authorship while preserving learning access—by making the tradeoff explicit and time-limited.
% TRANSFER_FUNCTION: Transfers the right to set printing monopoly from the Stationers' Company to Parliament and authors. Transfers economic rents from perpetual monopoly holders to limited-term copyright holders. Transfers intellectual access from exclusive control to eventual public domain. The statute moves authority over the conceptual boundary itself—who gets to define what copyright is.
% ABSENT_VOICES: Readers and future creators are structurally excluded from the legislative deliberation—they have no standing to testify or petition. The statute's framers gesture toward learning as a beneficiary, but learning itself cannot speak. Continental European legal traditions (which might conceive IP differently) are absent from the English parliamentary frame. Pirates and unlicensed printers are barred from the negotiation but would argue for open access.
% DISAPPEARANCE_RATIONALE: If the statute disappeared overnight, England would revert to perpetual monopoly under the Stationers' Company charter (or toward an unregulated printing market if the charter fell too). The entire subsequent architecture of copyright—the 14-year term, the renewal mechanism, the public domain concept—would not exist. The conceptual innovation that 'IP is a limited regulatory tool, not perpetual property' would be displaced by older frames of property or monopoly. Two centuries of legal development would be orphaned.
% FOUNDING_PROBLEM: How to incentivize authorship and publication while preventing perpetual monopoly that chokes learning and future creation. The prior frame (perpetual monopoly by chartered company) was seen as solving the incentive problem but at the cost of perpetual control, which Parliament concluded harmed learning.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary records and the statute's preamble explicitly state the problem and the solution. Stationers' Company records show perpetual monopoly enforcement and restriction of entry. Historians of printing and learning (including Adam Smith's later commentary) corroborate that perpetual monopoly did restrict learning access. The preamble itself, authored by Parliament, asserts this founding problem from outside any subsequent beneficiary's framing—it is the legislative voice, not a self-interested party.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).
:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38–0.44 across the interval) because the statute does extract monopoly rents for authors—copyright is not free—but it bounds the extraction temporally and tethers it to an explicit public-learning goal. Suppression is low (0.18–0.24) because the statute's enforcement relies on the conceptual frame (copyright IS limited) rather than on coercive suppression of alternatives—alternatives exist (piracy, unlicensed printing) but they operate within a clear rule-set that is defended by reference to the statute's stated purpose. Theater is low (0.12–0.16) because the statute's actual function (incentivizing authorship and learning) aligns with its stated function; it is not sustained by performative activity masking a different purpose. The measurements run on one shared grid; all metrics are authored at every examined time point. The slight rise in extractiveness mid-interval (1770–1790) reflects pressure from publishers to expand copyright scope beyond the statute's original bounds; the decline back to 0.38 by 1800 reflects the public-domain logic reasserting itself and publishers accepting the statutory frame as settled.
 *
 * PERSPECTIVAL GAP:
 *   This constraint presents a seat-divergence between Parliament/legal interpreters (who see coordination and public benefit) and the Stationers' Company (who see displacement and loss of authority). The gap is rooted in the reading itself: this reading is about conceptual innovation, not institutional reallocation. The institutional reading (sibling 1) would center the reallocation of monopoly from Stationers to authors and Parliament. The entangled reading (sibling 2) would argue that conceptual and institutional change are inseparable—you cannot think about the statute as pure concept without acknowledging the institutional displacement it performs. This reading isolates the conceptual component and treats the institutional change as downstream. The result: Parliament genuinely solved a coordination problem (incentive + learning) by inventing a new concept. The Stationers Company lost authority because the new concept displaces their old frame. Both facts are real; they describe different dimensions of the same event.
 *
 * DIRECTIONALITY LOGIC:
 *   Public learning and authors sit as beneficiaries (d near 0.0–0.2): the statute was designed for them, it creates benefits they collect. The Stationers' Company sits as a target (d near 0.8–1.0): the statute explicitly displaces their prior monopoly and restricts their future authority. Parliament is the agenda-setter (d ~0.5 from neutral institutional position, but actually more beneficiary-leaning because Parliament owns the authority to set the frame itself). Readers and future creators are beneficiaries of the design (d near 0.0), but their powerlessness and exclusion from deliberation keeps them unorganized and unable to secure their interests when the frame drifts. The derivation is straightforward: beneficiary declarations (public learning, authors) anchor low d; victim declaration (perpetual monopoly) anchors high d; exit options reflect structural constraint (Stationers trapped by the new frame, authors and learning made structurally dependent on copyright existing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no mandatrophy. The founding problem (perpetual monopoly chokes learning; need to incentivize authorship) remains live throughout the 90-year interval. The statute's stated solution (limited term, explicit public-benefit goal) continues to function as stated. Enforcement is conceptual (the frame is settled), not performative. Theater is low and stable, indicating the constraint is sustained by its actual coordination function rather than by symbolic maintenance. The slight extraction-rise mid-interval (1770–1790) reflects normal publisher pressure to expand copyright scope, which the statute's frame effectively resists; the return to baseline by 1800 shows the public-domain logic has won the dispute. No evidence of atrophy or inertial persistence; the constraint persists because it continues to solve the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_necessity_vs_institutional_accomplishment,
    'Is the new concept (copyright as limited regulatory tool) a genuine conceptual innovation that Parliament invented, or did Parliament simply articulate and institutionalize a concept that was already emerging in legal practice and merchant custom?',
    'Historical analysis of pre-statute legal and merchant texts: if the limited-term, public-benefit framing appears in prior writings (statute of monopolies, guild practices, mercantile correspondence), the concept was emerging prior to Parliament''s articulation; if it appears first in Parliament''s statute and subsequent legal commentary, Parliament''s articulation was the innovation.',
    'If pre-statute emergence: the statute is institutionalizing an existing concept, not inventing one; this reading would shift toward entangled (concept and institution co-emerge). If post-statute: this reading''s claim that Parliament created the conceptual space stands; copyright becomes thinkable because Parliament made it so.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_necessity_vs_institutional_accomplishment, empirical, 'Whether Parliament invented the conceptual frame or articulated one already emerging in practice.').

omega_variable(
    sibling_reading_boundary_contestation,
    'Can the conceptual innovation be separated from the institutional reallocation, or are they logically entangled such that one cannot hold the concept without also accepting the institutional shift?',
    'Counterfactual legal analysis: could Parliament have created the limited-term, public-benefit concept WITHOUT displacing the Stationers'' Company? Could the Stationers have accepted the new concept? Historical study of disputes over the concept in later legal tradition (e.g., Donaldson v. Beckett, 1774) to see whether parties accept the concept as settled or contest it as institutional displacement dressed up as innovation.',
    'If separable: this reading''s isolation of the conceptual component is structurally sound; the sibling institutional reading describes a different constraint. If entangled: this reading is a framing choice (legitimate but not the only reading); the entangled reading better captures the single event; both readings describe the same constraint from different angles, and the kernel admits both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_contestation, conceptual, 'Whether conceptual emergence and institutional reallocation are logically separable or co-constitutive.').

omega_variable(
    perpetual_monopoly_as_victim,
    'Is ''perpetual monopoly'' a meaningful victim in this constraint, or is it a discredited framing that Parliament had already chosen to displace before the statute?',
    'Analysis of pre-statute parliamentary and political debate: did Parliament struggle with the decision to limit monopoly (indicating perpetual monopoly was a live alternative), or was the decision to limit already consensus before drafting began (indicating perpetual monopoly was already a victim of prior conviction)?',
    'If live alternative: perpetual monopoly is a genuine victim, and the statute imposes a cost on perpetual-monopoly holders (the Stationers). If already displaced: ''perpetual monopoly'' is more a scapegoat than a victim; the real victim is the Stationers'' institutional authority, and the constraint is institutional reallocation, not conceptual innovation. This affects the classification: genuine coordination (this reading) vs. institutional transfer (sibling reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perpetual_monopoly_as_victim, empirical, 'Whether perpetual monopoly was a contested alternative or already discredited before the statute.').

omega_variable(
    reading_vs_entanglement_ambiguity,
    'This reading treats the statute as a conceptual innovation (one reading of a kernel). But is the kernel actually contestable, or is ''conceptual emergence'' simply what the statute is, with institutional change as a dependent fact?',
    'Subsequent legal history: if later judges and legislators re-interpret the statute''s concept differently (e.g., treating copyright as property rather than regulation), the kernel is genuinely contested and multiple readings are live. If the limited-regulatory framing persists as canonical and all disputes are about implementation details, the kernel may be settled and the ''reading'' framing over-complicates the picture.',
    'If contested: this reading''s isolation is valid and warranted. If settled: the entangled reading might better capture what happened—Parliament established a new regime, not merely offered one reading of a contested kernel. This would shift the classification framework from kernel-reading to straightforward constraint analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_entanglement_ambiguity, conceptual, 'Whether the statute''s meaning is genuinely contested (justifying the kernel-reading frame) or was settled from the outset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.12).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1730, 0.13).
narrative_ontology:measurement_basis(stat_tr_t1730, observed).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1750, 0.14).
narrative_ontology:measurement_basis(stat_tr_t1750, observed).
narrative_ontology:measurement(stat_tr_t1770, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1770, 0.16).
narrative_ontology:measurement_basis(stat_tr_t1770, observed).
narrative_ontology:measurement(stat_tr_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1790, 0.15).
narrative_ontology:measurement_basis(stat_tr_t1790, observed).
narrative_ontology:measurement(stat_tr_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(stat_tr_t1800, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.38).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1730, 0.4).
narrative_ontology:measurement_basis(stat_be_t1730, observed).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement_basis(stat_be_t1750, observed).
narrative_ontology:measurement(stat_be_t1770, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1770, 0.42).
narrative_ontology:measurement_basis(stat_be_t1770, observed).
narrative_ontology:measurement(stat_be_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1790, 0.44).
narrative_ontology:measurement_basis(stat_be_t1790, observed).
narrative_ontology:measurement(stat_be_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement_basis(stat_be_t1800, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.18).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1730, 0.2).
narrative_ontology:measurement_basis(stat_su_t1730, observed).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1750, 0.22).
narrative_ontology:measurement_basis(stat_su_t1750, observed).
narrative_ontology:measurement(stat_su_t1770, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1770, 0.23).
narrative_ontology:measurement_basis(stat_su_t1770, observed).
narrative_ontology:measurement(stat_su_t1790, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1790, 0.24).
narrative_ontology:measurement_basis(stat_su_t1790, observed).
narrative_ontology:measurement(stat_su_t1800, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1800, 0.22).
narrative_ontology:measurement_basis(stat_su_t1800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Statute of Anne kernel. The sibling readings (institutional_reallocation_reading, entangled_event_reading) describe the same statute through different interpretive lenses. All three share the same referent (the statute's text and operation) but author different ε values and beneficiary/victim structures because the readings themselves construct different constraints. The family decomposes a single historical event into three logically distinct claims: (1) this reading—the statute innovated a conceptual category; (2) institutional reading—the statute reallocated monopoly authority; (3) entangled reading—conceptual and institutional change are inseparable. Each reading is valid; they are not competing claims about a single fact but competing claims about what level of analysis captures the statute's true structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
