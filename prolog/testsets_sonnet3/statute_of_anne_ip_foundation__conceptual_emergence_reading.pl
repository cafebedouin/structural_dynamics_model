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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Statute of Anne as Conceptual Emergence of Time-Limited Copyright
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   This story authors the conceptual-emergence reading of the Statute of
 *   Anne (1710): the claim that the statute's real work was to install a new
 *   legal category — copyright as a time-limited regulatory grant justified
 *   by the encouragement of learning — where previously only unbounded
 *   proprietary claims (as asserted by the Stationers' Company) or
 *   unprotected copying were conceptually available. The beneficiary under
 *   this reading is public learning and the future public domain; the victim
 *   is the possibility of perpetual monopoly in a text, which this reading
 *   holds became legally unthinkable rather than merely legally defeated.
 *   This is one of three linked readings of the same kernel
 *   (statute_of_anne_ip_foundation): the entangled_event_reading holds the
 *   conceptual and institutional changes are inseparable, and the
 *   institutional_reallocation_reading holds the real action was a transfer
 *   of rights from the Stationers' Company to authors, with no new category
 *   required to explain it. Each reading is authored as its own constraint
 *   with its own epsilon; this file does not average across them.
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
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne as Conceptual Emergence of Time-Limited Copyright").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:has_sunset_clause(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, '385ec0b9-4852-4c94-9615-e67266d58466').
narrative_ontology:cs_kernel_codification('385ec0b9-4852-4c94-9615-e67266d58466', fixed_text).
narrative_ontology:cs_authority_grounding('385ec0b9-4852-4c94-9615-e67266d58466', lineage).
narrative_ontology:cs_interpretation_layer_present('385ec0b9-4852-4c94-9615-e67266d58466').
narrative_ontology:cs_reading_relation('385ec0b9-4852-4c94-9615-e67266d58466', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('385ec0b9-4852-4c94-9615-e67266d58466', statute_of_anne_ip_foundation__entangled_event_reading, influences).
narrative_ontology:cs_axiom('385ec0b9-4852-4c94-9615-e67266d58466', foundational, copyright_is_conceptually_novel_in_1710).
narrative_ontology:cs_axiom_status(copyright_is_conceptually_novel_in_1710, holdable).
narrative_ontology:cs_axiom_grounding('385ec0b9-4852-4c94-9615-e67266d58466', copyright_is_conceptually_novel_in_1710, empirically_contingent).
narrative_ontology:cs_axiom('385ec0b9-4852-4c94-9615-e67266d58466', secondary, learning_justification_is_constitutive_not_rhetorical).
narrative_ontology:cs_axiom_status(learning_justification_is_constitutive_not_rhetorical, holdable).
narrative_ontology:cs_axiom_grounding('385ec0b9-4852-4c94-9615-e67266d58466', learning_justification_is_constitutive_not_rhetorical, conventional).
narrative_ontology:cs_reference_frame('385ec0b9-4852-4c94-9615-e67266d58466', unbounded_proprietary_printing_right).
narrative_ontology:cs_drift_state('385ec0b9-4852-4c94-9615-e67266d58466', post_donaldson_v_beckett_1774, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('385ec0b9-4852-4c94-9615-e67266d58466', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_authors).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, printers_outside_stationers_monopoly).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_as_bounded_regulatory_grant).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, learning_as_the_object_of_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the new conceptual frame, works enter the public domain after a fixed term, expanding what can be freely printed, taught from, and built upon. Before the statute's conceptual reframing, the prevailing frame treated the printing right as a form of property with no articulated terminus, so the public had no structural claim to eventual free access; this reading holds that the statute installed that claim as a thinkable legal category for the first time.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    powerless, generational, constrained, national).

% Gain, for the first time in this reading's account, a legally cognizable authorial interest distinct from the bookseller's trade privilege — a term-limited grant framed around encouraging learning rather than around perpetual proprietary control. They benefit from the category's existence even though the specific 14/21-year terms are modest.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, future_authors, beneficiary,
    moderate, generational, constrained, national).

% Once the conceptual space for a bounded, expiring right exists, printers who were previously locked out of the Stationers' perpetual claim gain a horizon at which titles become printable by anyone. Their situation improves not because they seized an existing right (that is the sibling institutional-reallocation story) but because a new category — expiring regulatory grant — came into legal existence that did not exist to be seized before.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, printers_outside_stationers_monopoly, beneficiary,
    moderate, biographical, constrained, national).

% Booksellers and their allies who had operated on the premise that a printing right, once vested, persisted indefinitely, lose the conceptual ground for that premise. In this reading, what is taken from them is not a specific asset but the framework itself — the statute makes perpetual property in a text unthinkable as a matter of positive law, which is a deeper loss than losing any single title's exclusivity.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_claimants, payer,
    organized, generational, constrained, national).

% Drafts and enacts the statute, articulating for the first time a rationale — encouragement of learning — that frames the grant as instrumental and temporary rather than as recognition of a pre-existing natural or perpetual property. Parliament administers the new category going forward through subsequent legislation and could, in principle, revert to an undifferentiated property frame, but the conceptual move itself proves durable.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Examine the statute's text, parliamentary debates, and subsequent case law (e.g. Donaldson v. Beckett) to adjudicate whether a genuinely new conceptual category was created in 1710 or whether the language of 'learning' was rhetorical cover for a reallocation already underway. This reading is their preferred account; other historians hold the sibling readings.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to justify limiting a printing right at all: by naming 'encouragement of learning' as the public good the grant serves, the statute makes a term-limited, purpose-bound right conceptually available where previously only unbounded property or unbounded piracy were thinkable options.
% TRANSFER_FUNCTION: Moves the available conceptual vocabulary for describing textual rights from 'property, perpetual by default' to 'regulatory grant, temporary by design, justified by public learning' — the transfer is of legal categories and their attendant expectations, not primarily of specific assets between named parties.
% ABSENT_VOICES: Provincial printers and readers outside London, and the illiterate poor for whom 'encouragement of learning' was aspirational rather than descriptive, are not represented in the parliamentary record; their potential objection — that the term limits were still too long, or that access remained priced beyond reach — is absent from the debate this reading relies on.
% DISAPPEARANCE_RATIONALE: If the conceptual category the statute is credited with creating had never existed, subsequent copyright law would have no ready-made frame of 'bounded grant for a public purpose' to draw on; later terms, fair-use-like doctrines, and the eventual public domain would have had to be argued from a different and less hospitable starting premise, most likely from within an unbounded-property frame that resists limitation by design.
% FOUNDING_PROBLEM: The problem was the absence of any legal vocabulary for a printing right that was neither unbounded property nor mere unprotected copying — a gap that left both the incentive to publish and the eventual return of works to public use unaddressed in positive law.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary groups (e.g. scholars of the Donaldson v. Beckett litigation and comparative civil-law copyright origins) attest that the conceptual category did emerge from the statute's text and was later contested and partially reaffirmed by courts; other historians in the same outside position argue the 'learning' language was rhetorical and the real change was institutional reallocation (see sibling reading) — the corroboration itself is split along scholarly lines, which is why founding_problem_status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored low-moderate (0.28) because the reading treats the statute's cost to perpetual-monopoly claimants as the loss of a framework, not an ongoing extractive transfer — the term-limited grant itself, once installed, extracts modestly from the public (the term is not free) but far less than an unbounded property regime would. Suppression (0.35) reflects that the new frame did require enforcement against continued perpetual-property claims (Stationers' litigation persisted for decades), but the suppression is directed at defending a conceptual boundary, not at extracting rents from a captured population. Theater ratio stays low and roughly flat (0.10-0.15) because the coordination function — a real, cited, workable term-limit doctrine — is not primarily performative; the 'encouragement of learning' language is treated in this reading as doing genuine structural work, not cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Reading public and future authors are coded as beneficiaries because the new category gives them a legal claim — eventual free access, a term-bound authorial interest — that did not previously exist to be claimed. Perpetual monopoly claimants are coded as payer/victim because what is taken from them, under this reading, is the conceptual ground for their prior claim, not a specific seized asset (that would be the institutional_reallocation_reading's framing). Parliament sits as agenda_setter with analytical exit because it authors and can in principle revise the category, though the durability of the conceptual move constrains even Parliament's freedom to simply reverse it.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is chosen because the statute's own justification is explicitly transitional: rights lapse after a fixed term (14 years, renewable once) precisely so that the transition from restricted to public availability occurs on a schedule, not by exception. This prevents mislabeling the arrangement as pure extraction (a snare would require the term limits to be cover for indefinite renewal capture) or as pure coordination with no cost (a rope would understate that perpetual-monopoly claimants genuinely lose something real under this reading) — the sunset clause is the structural fact that keeps this a scaffold rather than either extreme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_novelty_vs_relabeled_transfer,
    'Did the Statute of Anne create a conceptually new category of right (time-limited regulatory grant for learning), or did it merely relabel a transfer of an existing proprietary right from the Stationers'' Company to authors, using ''learning'' as legitimating rhetoric?',
    'Close textual and doctrinal history: examine whether courts and subsequent legislation (e.g. Donaldson v. Beckett 1774) treated the term limit as enforcing a genuinely new conceptual boundary against renewed perpetual claims, or as a procedural adjustment within a continuously-held property frame.',
    'If the category is genuinely novel, this reading''s classification (scaffold with public learning as beneficiary) holds; if it is relabeled transfer, the institutional_reallocation_reading''s classification (a different beneficiary/victim structure, likely tangled_rope or snare toward the Stationers'' Company) is the more accurate account of the same historical event, and this story''s high-level type claim would not survive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_novelty_vs_relabeled_transfer, conceptual, 'Whether the statute''s conceptual novelty is real or rhetorical cover for reallocation.').

omega_variable(
    committer_structure_kernel_disambiguation,
    'This constraint is one of three readings of the statute_of_anne_ip_foundation kernel — conceptual_emergence, institutional_reallocation, and entangled_event. Where exactly is the disagreement located structurally?',
    'The disagreement is located in whether a ''new legal category'' is a separable causal event from ''a transfer of institutional occupancy of the printing-rights space.'' The entangled_event_reading holds these cannot be disentangled even analytically; this reading and the institutional_reallocation_reading each hold one dimension is primary and the other secondary or epiphenomenal.',
    'Adopting the entangled_event_reading instead would mean this story''s clean beneficiary/victim split (public learning vs. perpetual monopoly) is not defensible as a standalone causal account — the reallocation and the conceptual shift would need to be treated as one indivisible event with a single, harder-to-decompose extraction profile, likely with a different epsilon than either single-dimension reading authors alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_disambiguation, conceptual, 'Locates where the three sibling readings of the kernel diverge and what adopting a different reading would change.').

omega_variable(
    term_limit_erosion_risk,
    'Does the conceptual category installed in 1710 remain stable over subsequent legislative extensions of copyright term, or does the ''limited grant for learning'' framing erode into something closer to the perpetual property it was meant to displace?',
    'Track subsequent statutory term extensions (1814, 1842, 1911, and beyond) against the founding rationale; if terms extend indefinitely while retaining the ''learning'' justification, the category may have been captured rather than merely applied.',
    'If later extensions substantially erode the term limit''s bite, the conceptual_emergence_reading''s claim that the category durably serves public learning is undermined retrospectively, though the 1710 conceptual innovation itself remains a distinct historical fact from its later erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_limit_erosion_risk, empirical, 'Whether the founding conceptual category survives or is captured by later term extensions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1710, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.1).
narrative_ontology:measurement(stat_tr_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1720, 0.11).
narrative_ontology:measurement(stat_tr_t1731, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1731, 0.13).
narrative_ontology:measurement(stat_tr_t1743, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1743, 0.14).
narrative_ontology:measurement(stat_tr_t1759, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1759, 0.15).
narrative_ontology:measurement(stat_tr_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1774, 0.15).

% Extraction over time
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.22).
narrative_ontology:measurement(stat_be_t1720, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1720, 0.24).
narrative_ontology:measurement(stat_be_t1731, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1731, 0.27).
narrative_ontology:measurement(stat_be_t1743, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1743, 0.3).
narrative_ontology:measurement(stat_be_t1759, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1759, 0.29).
narrative_ontology:measurement(stat_be_t1774, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1774, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statute_of_anne_ip_foundation__conceptual_emergence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, entangled_event_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the statute_of_anne_ip_foundation kernel. institutional_reallocation_reading treats the same historical event as a transfer of rights from the Stationers' Company to authors with no new conceptual category required; entangled_event_reading treats the conceptual and institutional dimensions as inseparable. Each carries its own epsilon and beneficiary/victim structure per the epsilon-invariance principle; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
