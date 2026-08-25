% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence — Thinkability Reading (1710 Statute of Anne)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   The thinkability reading of the ip_category_emergence kernel holds that
 *   the Statute of Anne (1710) did not merely adjust an existing privilege —
 *   it instantiated a new legal category: 'ownable expression' as a coherent
 *   object of property law. Before 1710, disputes over printing used the
 *   vocabulary of royal prerogative, guild privilege, and censorship; after
 *   1710, the vocabulary of 'copyright,' 'literary property,' and 'the
 *   author's right' became legally thinkable and deployable in courts. This
 *   category emergence is the constraint: once the category exists, it
 *   structures all subsequent disputes, legislation, and commercial practice,
 *   even as its content (term, scope, subject matter) drifts. The constraint
 *   is a tangled rope: it coordinates a national book trade (genuine
 *   coordination) while extracting monopoly rents from the public domain and
 *   follow-on creators (asymmetric extraction), and it requires active
 *   enforcement (courts, customs, later digital locks) to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.65).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.55).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence — Thinkability Reading (1710 Statute of Anne)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '83644c44-8769-4732-9f4d-9beedaba09f0').
narrative_ontology:cs_kernel_codification('83644c44-8769-4732-9f4d-9beedaba09f0', formalized).
narrative_ontology:cs_authority_grounding('83644c44-8769-4732-9f4d-9beedaba09f0', lineage).
narrative_ontology:cs_interpretation_layer_present('83644c44-8769-4732-9f4d-9beedaba09f0').
narrative_ontology:cs_reading_relation('83644c44-8769-4732-9f4d-9beedaba09f0', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('83644c44-8769-4732-9f4d-9beedaba09f0', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('83644c44-8769-4732-9f4d-9beedaba09f0', foundational, intellectual_property_category_emergent_1710).
narrative_ontology:cs_axiom_status(intellectual_property_category_emergent_1710, holdable).
narrative_ontology:cs_axiom_grounding('83644c44-8769-4732-9f4d-9beedaba09f0', intellectual_property_category_emergent_1710, conventional).
narrative_ontology:cs_axiom('83644c44-8769-4732-9f4d-9beedaba09f0', secondary, pre_1710_disputes_lacked_ip_vocabulary).
narrative_ontology:cs_axiom_status(pre_1710_disputes_lacked_ip_vocabulary, holdable).
narrative_ontology:cs_axiom_grounding('83644c44-8769-4732-9f4d-9beedaba09f0', pre_1710_disputes_lacked_ip_vocabulary, empirically_contingent).
narrative_ontology:cs_reference_frame('83644c44-8769-4732-9f4d-9beedaba09f0', pre_statutory_privilege_regime).
narrative_ontology:cs_drift_state('83644c44-8769-4732-9f4d-9beedaba09f0', contemporary_copyright_maximalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83644c44-8769-4732-9f4d-9beedaba09f0', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, stationers_publishers).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, legal_profession).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, state_crown).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, public_domain_users).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, follow_on_creators).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, readers_audiences).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors_creators).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, stationers_publishers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, authors_creators).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, statutory_copyright_as_coherent_legal_category).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, expression_as_ownable_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Statute of Anne (1710) creating the first statutory copyright regime. Framed it as 'an Act for the Encouragement of Learning' — vesting 'copy right' in authors for limited terms (14+14 years) to break the Stationers' perpetual monopoly. Collected registration fees and gained a regulatory lever over the press.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliament_crown, agenda_setter,
    institutional, generational, arbitrage, national).

% The Stationers' Company held a royal charter granting perpetual printing privileges. The Statute of Anne formally ended their monopoly but in practice they captured the new system: authors assigned rights to publishers for publication, and publishers lobbied for term extensions (1735, 1774 Donaldson v Beckett). They bear registration and enforcement costs but collect the lion's share of monopoly rents.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, stationers_publishers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, stationers_publishers, payer).

% Formally the new rights-holders under the Statute, but in practice rarely retained rights — economic necessity forced assignment to publishers. Gained legal recognition as 'proprietors' of their intellectual labor, a novel status. Bear the cost of dependence on publishers for distribution; exit means patronage or obscurity.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors_creators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, authors_creators, payer).

% A new field of 'literary property' litigation emerged — Donaldson v Beckett (1774), Millar v Taylor (1769). Lawyers, judges, and treatise writers (Blackstone) built careers interpreting the novel category. The ambiguity of 'common law copyright' vs statutory right generated decades of billable dispute.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_profession, beneficiary,
    organized, generational, arbitrage, national).

% Pre-1710, the public could freely reprint, adapt, and circulate works once the Stationers' monopoly lapsed or was ignored. Post-1710, a statutory barrier enclosed 28 years (later extended) of every published work. No organized representation in Parliament; their loss is diffuse and invisible in the legislative record.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain_users, payer,
    powerless, immediate, trapped, national).

% Translators, abridgers, dramatists, and later encyclopedists found their raw material legally fenced. The 'idea/expression' distinction did not yet exist — the category 'ownable expression' swallowed adaptation. Exit means creating from scratch or risking infringement.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, follow_on_creators, payer,
    powerless, biographical, constrained, national).

% Book prices remained high; the Stationers used copyright to maintain price-fixing cartels (the 'conger'). Cheap reprints that would have emerged from competition were legally blocked. No exit — literacy required books, books required the regulated trade.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, readers_audiences, payer,
    powerless, immediate, trapped, national).

% Scotland (no copyright until 1710, then divergent case law), Ireland, American colonies, and later France (1791) and German states watched the English experiment. They adopted, rejected, or modified the category. Their comparative experience is the control group for whether the category was 'necessary' for learning.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, competing_jurisdictions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Created a standardized, transferable, time-limited property right in printed expression — replacing ad hoc royal privileges and guild monopolies with a general legal category that enabled a national market in books, attracted capital to publishing, and gave the state a regulatory hook instead of a censorship hook.
% TRANSFER_FUNCTION: Moves monopoly rent from readers and follow-on creators (who pay higher prices and lose adaptation freedom) to publishers and the legal profession (who collect the rent and the litigation fees), with a residual flow to authors (who gained formal recognition but rarely economic independence) and the state (registration fees, trade regulation).
% ABSENT_VOICES: The reading public, provincial printers outside the Stationers' Company, Scottish and Irish publishers who operated under different rules, and women writers (excluded from the Stationers' livery) — none had standing in the 1710 debates. Their absence is structural: the Act was negotiated between the Crown, the Stationers, and a handful of prominent authors.
% DISAPPEARANCE_RATIONALE: If the statutory copyright category vanished overnight, the entire edifice of modern IP — term extensions, moral rights, neighboring rights, digital enforcement, international treaties (Berne, TRIPS) — would lose its foundational conceptual anchor. The publishing industry would revert to contract and trade secrecy; the public domain would expand to all published works; the legal specialty of 'IP law' would dissolve.
% FOUNDING_PROBLEM: The Stationers' perpetual monopoly (rooted in 1557 royal charter) had become a censorship instrument and a price-fixing cartel. The Crown wanted to break the Stationers' political power; authors wanted recognition; the public wanted cheaper books. The Statute of Anne was the compromise: a limited statutory right replacing perpetual privilege.
% FOUNDING_PROBLEM_CORROBORATION: The Stationers' monopoly was indeed broken — Donaldson v Beckett (1774) confirmed copyright was statutory, not common law, and expired. Term was 28 years. But the category 'ownable expression' persisted and expanded: terms lengthened (1842: 42 years or life+7; 1911: life+50; 1995: life+70), subject matter widened (engravings 1735, performances 1833, photographs 1862, software 1980s). The founding problem (Stationers' perpetual monopoly) is dead; the arrangement it birthed metastasized. Corroboration: Ronan Deazley (legal historian), Isabella Alexander (copyright historian), and the UK IPO's own legislative history all attest the 1710 compromise was overtaken by successive rent-extensions — none of the original beneficiaries (public, authors) consented to the modern regime.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron_think+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the steady expansion of term and scope far beyond the 1710 bargain — each extension transfers value from the public domain to rights-holders (mostly corporate successors). Suppression (0.55) is moderate: the constraint operates through civil litigation and state enforcement, not totalitarian control; alternatives (patronage, open access, commons-based production) persist at the margins. Theater ratio (0.3) is low-moderate: the 'encouragement of learning' justification remains the stated purpose, but the gap between that purpose and the life+70 corporate term is visible. Accessibility collapse (0.5) is partial: the public domain still exists (works published >70 years ago), and fair dealing/fair use carve out space. Resistance (0.4) has been consistent but ineffective — the public domain movement, pirate libraries, and reform proposals have not reversed the expansionary trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The publisher seat experiences this as a rope (coordination that enables their business model). The author seat experiences it as a tangled rope (some recognition, but structural dependency). The public/reader seat experiences it as a snare (extraction with no consent). The engine will compute these divergences from the structural data — the thinkability reading claims the CATEGORY ITSELF is the constraint, not any particular term length.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament/Crown (agenda_setter) sits near the beneficiary end (d ~0.15): they gained a regulatory tool and broke a rival power center. Stationers/publishers (beneficiary) are the primary capture point (d ~0.1): they collect the rents and write the lobbying script. Authors (dual) sit near symmetric (d ~0.5): formal recognition vs economic dependence. Legal profession (beneficiary) is a clear winner (d ~0.2): new field, steady work. Public domain users, follow-on creators, readers (payers) are targets (d ~0.85-0.95): diffuse costs, no exit, no representation. Competing jurisdictions (observer) are analytical (d ~0.5): they observe and choose whether to adopt the category.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' perpetual monopoly) is dead — confirmed by Donaldson v Beckett and historical record. But the arrangement persists and expands. This is classic mandatrophy: the constraint's mandate ('encouragement of learning' via limited monopoly) has been inverted — the monopoly is no longer limited, and learning is not the measurable beneficiary. The thinkability reading highlights that the CATEGORY'S COHERENCE is what persists: once 'expression is property' becomes thinkable, every expansion is just 'applying the principle.' The mandate didn't atrophy; the category colonized the mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_vs_instrument_ambiguity,
    'Is the ''thinkability'' of IP as a category a genuine conceptual break (Mountain-like: once thought, cannot be unthought) or a constructed legal instrument that could have been otherwise (Rope/Snare: contingent on power)?',
    'Counterfactual legal history: trace whether Scottish, French, and American jurisdictions developed equivalent categories without the 1710 precedent. If they did, the category is convergent (Mountain-like); if they diverged, it is contingent (constructed).',
    'If convergent, the constraint has Mountain-like naturalness — the category was ''waiting to be discovered.'' If contingent, it is a constructed constraint with beneficiaries (publishers, lawyers) — FSM candidate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_vs_instrument_ambiguity, conceptual, 'Whether IP category emergence is discovery or invention.').

omega_variable(
    extraction_coordination_boundary_1710,
    'How much of the 1710-1774 period''s value flow was genuine coordination (market formation, learning encouragement) vs extraction (Stationers'' cartel continuation under new legal cover)?',
    'Economic history of book prices, title diversity, and provincial printing 1710-1774 vs 1690-1710. Compare Scottish uncopyrighted market (1710-1774) as natural experiment.',
    'If coordination dominated early, the tangled_rope claim holds (genuine function + later extraction). If extraction dominated from 1710, the claim shifts toward snare (coordination was cover from the start).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary_1710, empirical, 'Whether the Statute of Anne''s initial operation was net-coordinating or net-extractive.').

omega_variable(
    thinkability_vs_first_holding_boundary,
    'Are ''category emergence'' (thinkability) and ''author as rights-holder'' (first-holding) structurally distinct claims, or two framings of the same legal event?',
    'Formal analysis: can a legal system have ''ownable expression'' as a category WITHOUT ''author as initial rights-holder''? (e.g., employer-owned works, state-owned works). If yes, the readings are independent; if no, they are coupled.',
    'If independent, the kernel has two distinct constraint stories with different ε. If coupled, they are one constraint with two analytical angles — the ε-invariance principle would demand decomposition or merger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_vs_first_holding_boundary, conceptual, 'Structural independence of the two sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1710, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.2).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t1842, ip_category_emergence__thinkability_reading, theater_ratio, 1842, 0.25).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t1911, ip_category_emergence__thinkability_reading, theater_ratio, 1911, 0.28).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t1995, ip_category_emergence__thinkability_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_tr_t2024, ip_category_emergence__thinkability_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.35).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.45).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t1842, ip_category_emergence__thinkability_reading, base_extractiveness, 1842, 0.55).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t1911, ip_category_emergence__thinkability_reading, base_extractiveness, 1911, 0.6).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t1995, ip_category_emergence__thinkability_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_be_t2024, ip_category_emergence__thinkability_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.4).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.45).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t1842, ip_category_emergence__thinkability_reading, suppression_requirement, 1842, 0.5).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t1911, ip_category_emergence__thinkability_reading, suppression_requirement, 1911, 0.52).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t1995, ip_category_emergence__thinkability_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement(ip_category_emergence__thinkability_reading_su_t2024, ip_category_emergence__thinkability_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.02).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This kernel decomposes into three constraint stories: thinkability_reading (category emergence, ε=0.65), first_holding_reading (occupancy change/author-as-rights-holder, ε likely lower — coordination-heavy), and synchronic_diachronic_seam (formal independence test, ε near 0 — analytical claim). The thinkability reading is upstream: the category must exist before authors can hold rights in it. The first-holding reading is downstream: it assumes the category and asks who occupies the initial holder slot. The synchronic_diachronic_seam is a meta-constraint testing whether the two readings are structurally coupled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, organized, 0.1).
constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
