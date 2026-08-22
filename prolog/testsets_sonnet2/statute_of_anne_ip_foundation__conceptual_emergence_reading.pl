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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne (1710) as Conceptual Emergence: Copyright Reconceived as Time-Limited Learning Instrument
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as the moment copyright became
 *   conceivable as a distinct, time-bound regulatory instrument serving
 *   public learning rather than either a guild registration privilege or an
 *   unbounded natural-property claim. Under this reading the statute's chief
 *   structural effect is not redistribution among existing claimants but the
 *   creation of a new point in conceptual space: 'temporary entitlement in
 *   exchange for eventual public access.' The beneficiary is public learning
 *   (readers, future authors, printers outside the old registration
 *   monopoly); the victim is the possibility of perpetual monopoly, which the
 *   new category renders not just legally defeated but conceptually
 *   unavailable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.28).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne (1710) as Conceptual Emergence: Copyright Reconceived as Time-Limited Learning Instrument").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '12729742-82e6-46d0-8323-689f939c3fc9').
narrative_ontology:cs_kernel_codification('12729742-82e6-46d0-8323-689f939c3fc9', formalized).
narrative_ontology:cs_authority_grounding('12729742-82e6-46d0-8323-689f939c3fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('12729742-82e6-46d0-8323-689f939c3fc9').
narrative_ontology:cs_reading_relation('12729742-82e6-46d0-8323-689f939c3fc9', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('12729742-82e6-46d0-8323-689f939c3fc9', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('12729742-82e6-46d0-8323-689f939c3fc9', foundational, copyright_is_definitionally_time_bounded).
narrative_ontology:cs_axiom_status(copyright_is_definitionally_time_bounded, holdable).
narrative_ontology:cs_axiom_grounding('12729742-82e6-46d0-8323-689f939c3fc9', copyright_is_definitionally_time_bounded, conventional).
narrative_ontology:cs_axiom('12729742-82e6-46d0-8323-689f939c3fc9', foundational, conceptual_categories_can_emerge_from_legislation_rather_than_merely_redistribute_existing_ones).
narrative_ontology:cs_axiom_status(conceptual_categories_can_emerge_from_legislation_rather_than_merely_redistribute_existing_ones, holdable).
narrative_ontology:cs_axiom_grounding('12729742-82e6-46d0-8323-689f939c3fc9', conceptual_categories_can_emerge_from_legislation_rather_than_merely_redistribute_existing_ones, conventional).
narrative_ontology:cs_reference_frame('12729742-82e6-46d0-8323-689f939c3fc9', guild_registration_and_natural_property_taxonomy).
narrative_ontology:cs_drift_state('12729742-82e6-46d0-8323-689f939c3fc9', post_donaldson_v_becket_settlement, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('12729742-82e6-46d0-8323-689f939c3fc9', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, printers_outside_stationers_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_copyright_claimants).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains access to works once the fixed statutory term (14 years, renewable once) expires, at which point works enter the public domain and can be reprinted cheaply by any printer. Under the prior regime of perpetual common-law copyright asserted by the Stationers, no such entry point existed. The public has no direct role in enforcing the term but is the party for whom the term's existence matters.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    powerless, generational, constrained, national).

% Benefit from a legal category that recognizes authorial (not just publisher) entitlement, however time-limited, and from a public domain that supplies raw material for new derivative and referential work once terms lapse. They inherit a conceptual toolkit — 'this is a limited-term entitlement in exchange for eventual public access' — that did not previously exist as thinkable legal furniture.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_authors, beneficiary,
    moderate, civilizational, mobile, national).

% Gain the ability to print works once their term lapses, breaking the closed-shop registration system the Stationers' Company had used to control the trade. Their exit option under the old regime was effectively none within the licensed trade; the new statute opens a route to lawful printing outside that guild structure once terms run out.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, printers_outside_stationers_monopoly, beneficiary,
    moderate, biographical, mobile, national).

% Booksellers and their allied jurists who continued, for decades after 1710, to argue a perpetual common-law copyright survived independent of the statute. The statute's term structure is the direct negation of their claim: if a conceptual space exists in which copyright is inherently and definitionally time-limited, their position becomes not merely defeated on the facts but conceptually incoherent — there is no perpetual-property slot left in the taxonomy for their claim to occupy.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_copyright_claimants, payer,
    organized, generational, constrained, national).

% Held closed-shop registration control over the printing trade under royal licensing and the Stationers' own bylaws before 1710. The new conceptual category of author-centered, term-limited copyright displaces the framework in which their registry was the definitive record of perpetual entitlement; they must now operate inside a legal space that was not built around their institutional convenience.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_incumbents, payer,
    organized, biographical, constrained, national).

% Enacts the statute, creating the term-limited category by legislative act. Under this reading, Parliament is not merely reallocating an existing slot between two occupants (Stationers vs. authors) but authoring a slot that did not exist before — 'temporary, learning-oriented, publicly terminating entitlement' as a distinct legal kind, alongside real property and perpetual privilege.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_1710, agenda_setter,
    institutional, generational, analytical, national).

% Analyze whether 1710 marks genuine conceptual emergence (a new legal kind becoming thinkable) versus mere reallocation of an existing property right between incumbents. This story is authored from the conceptual-emergence seat; other seats read the same statute as reallocation or as an inseparable single event.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, diffuse).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to incentivize authorship and printing investment while guaranteeing that knowledge eventually becomes freely reproducible — a genuinely new conceptual solution (bounded, purpose-linked entitlement) to a problem the prior perpetual-privilege regime could not solve because it had no terminating logic built in.
% TRANSFER_FUNCTION: Moves the entitlement to reproduce a work, at the end of a fixed term, from whoever held it (author or assignee) to the public at large; before the term ends it moves printing revenue from unlicensed printers to the rights-holder, but only for a bounded period rather than in perpetuity.
% ABSENT_VOICES: Individual authors as a class had little direct voice in the 1710 negotiations, which were driven by the Stationers seeking continued protection and Parliament seeking to break the guild monopoly; the eventual authorial benefit is a byproduct of the new conceptual category more than a demanded outcome. Future readers and printers who would benefit once the public domain filled in were not present as an organized interest at all.
% DISAPPEARANCE_RATIONALE: If the conceptual category the statute inaugurated — copyright as inherently time-limited and purpose-linked to learning — had never come into existence, later doctrine would have had no ready framework to resist claims of perpetual common-law copyright (as very nearly happened in Millar v Taylor before Donaldson v Becket settled it), and the public domain as a legal concept would have had to be invented from different materials or not at all.
% FOUNDING_PROBLEM: There was no existing legal concept for a limited-duration, publicly terminating authorial entitlement; copyright before 1710 was either a guild registration privilege or an unbounded claim under natural/common law reasoning, with no principled stopping point built into the concept itself.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside any incumbent stationer or publisher interest (e.g., analyses of Donaldson v Becket 1774 and subsequent doctrinal histories) attest that the conceptual innovation of bounded, purpose-linked copyright was substantially settled by the late eighteenth century and has not been seriously recontested as a category since, even though the specific term lengths and scope have been repeatedly renegotiated.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate-low (0.28-0.32) because the fourteen-year (renewable) term is a genuine bounded transfer with a hard public-domain endpoint, not an open-ended rent; it declines slightly over the interval as the concept stabilizes and litigation (Millar v Taylor, Donaldson v Becket) settles the terminating logic rather than reopening it. Suppression starts moderate (0.55) reflecting the active resistance from perpetual-copyright claimants and the Stationers' registry apparatus in the early decades, and falls (to 0.35) once the category is judicially confirmed and the old claim becomes conceptually, not merely legally, foreclosed. Theater ratio stays low throughout — the statutory term and registration formalities are functional, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Parliament) and the beneficiary seats (reading public, future authors, non-Stationer printers), the statute looks like coordination: a workable, time-bound solution to an incentive/access problem that previously had no principled answer. From the payer seats (perpetual-copyright claimants, Stationers incumbents), the same statute is experienced as extraction of a claimed perpetual entitlement — worse, as the erasure of the conceptual ground on which that claim stood. The engine should register this seat divergence directly from the declared beneficiary/victim structure without needing the reading to editorialize about which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Reading public, future authors, and non-Stationer printers are declared beneficiaries because the new bounded category is what gives them, respectively, eventual free access, a legal toolkit that did not exist before, and an entry point into the trade — low d, benefit-side. Perpetual-copyright claimants and Stationers incumbents are declared victims because the terminating logic of the new category directly negates the substance of their prior claim and institutional position — high d, target-side. Parliament sits as agenda_setter/analytical rather than beneficiary or payer since it authors the category without itself collecting or paying under it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no legal concept existed for bounded, purpose-linked authorial entitlement — is authored as dead: the conceptual gap it filled has been closed for roughly three centuries and is not seriously recontested as a category (only its parameters, e.g. term length, remain contested). Because disappearance_verdict is world_rearranges (removing the concept would reopen exactly the taxonomy gap it filled) while founding_problem_status is dead, this is NOT a zombie/capture pattern — the arrangement is not persisting past its function; rather the concept succeeded so completely that removing it would require re-solving a problem the culture has forgotten needed solving. This is the correct scaffold reading: the transitional category (bounded rather than perpetual protection) became permanent legal furniture, but its *function* — supplying the public-domain endpoint — remains live, which is why it should not be reclassified as inert piton despite its age.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_emergence_vs_reallocation_locus,
    'Is the correct locus of the statute''s effect the emergence of a new legal CONCEPT (bounded, purpose-linked copyright as a distinct kind), or merely the REALLOCATION of an existing property-like entitlement from the Stationers'' Company to authors, with the term limit being an incidental feature rather than the core innovation?',
    'Doctrinal history tracing whether courts and commentators in the decades after 1710 treated the term limit as definitional to copyright''s new nature (supporting conceptual emergence) or as a negotiated compromise attached to a pre-existing kind of right (supporting reallocation). The Millar v Taylor / Donaldson v Becket sequence is the key evidentiary window: if courts explicitly grappled with whether a NEW kind of right had been created, that supports emergence; if they treated it purely as a scope-of-transfer question, that supports reallocation.',
    'If reallocation is the true locus, this story''s beneficiary/victim structure (public learning vs. perpetual monopoly) collapses into the institutional_reallocation_reading''s structure (authors vs. Stationers), and this reading''s claim of a genuinely NEW conceptual space becomes overstated — the ε and classification here would need revision toward that sibling''s terms rather than standing as an independent constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_emergence_vs_reallocation_locus, conceptual, 'Whether the statute''s core effect is genuine conceptual innovation or incidental term-limiting on a reallocated existing right.').

omega_variable(
    public_domain_as_beneficiary_or_byproduct,
    'Is ''public learning'' / ''the reading public'' a genuine intended beneficiary of the 1710 Act''s conceptual innovation, or a byproduct that later legal and cultural narratives retroactively centered to legitimate a statute actually driven by trade politics between booksellers and Parliament?',
    'Parliamentary debate records and petitions from 1709-1710 could establish whether public access / learning was an articulated goal at the time of drafting, versus a post-hoc justification developed once the Stationers'' perpetual claims needed defeating in later litigation.',
    'If public learning was a genuine drafting goal, the coordination function claimed here (rope/scaffold framing) is well-grounded. If it was retroactively supplied, the conceptual_emergence_reading''s beneficiary declaration is itself a piece of legitimating narrative rather than a structural fact, which would push this reading''s classification closer to tangled_rope (coordination story as partial cover for a trade-political settlement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_domain_as_beneficiary_or_byproduct, empirical, 'Whether public learning was an authored goal of the statute or a later legitimating narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(stat_tr_t100, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 100, 0.13).
narrative_ontology:measurement(stat_tr_t150, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 150, 0.14).
narrative_ontology:measurement(stat_tr_t200, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(stat_tr_t250, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 250, 0.15).
narrative_ontology:measurement(stat_tr_t300, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(stat_be_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(stat_be_t100, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(stat_be_t150, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 150, 0.27).
narrative_ontology:measurement(stat_be_t200, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement(stat_be_t250, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 250, 0.28).
narrative_ontology:measurement(stat_be_t300, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 300, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(stat_su_t100, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(stat_su_t150, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 150, 0.37).
narrative_ontology:measurement(stat_su_t200, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(stat_su_t250, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 250, 0.35).
narrative_ontology:measurement(stat_su_t300, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 300, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This story, institutional_reallocation_reading, and entangled_event_reading form a three-member constraint family reading the same 1710 statute through different structural lenses: conceptual emergence (a new legal kind is authored; low-moderate ε centered on the public-learning/perpetual-monopoly axis), institutional reallocation (an existing entitlement moves from the Stationers' Company to authors; ε centered on the incumbent-guild/author axis), and entangled event (the two dimensions are treated as analytically inseparable; ε reflects a blended reading). Each carries its own ε, beneficiaries, victims, and claimed_type per the ε-invariance principle — they are not the same constraint measured three ways but three distinct constraints sharing a documentary origin, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
