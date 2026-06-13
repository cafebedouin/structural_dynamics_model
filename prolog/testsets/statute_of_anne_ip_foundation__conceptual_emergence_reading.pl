% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   The Statute of Anne (1710) is read in this constraint story as a
 *   conceptual innovation: it created a new legal vocabulary in which
 *   copyright could be understood as a limited regulatory tool rather than a
 *   perpetual property right. Before the statute, IP protection existed (the
 *   Stationers' Company monopoly), but the concept 'IP as distinct category'
 *   with built-in temporal limits did not have statutory form. The statute's
 *   preamble articulates the new frame: copyright is granted 'for the
 *   encouragement of learning,' with a fixed term (fourteen years, renewable
 *   for another fourteen), after which works return to the public domain.
 *   This reading isolates the conceptual emergence from the institutional
 *   reallocation question: whether the statute merely shifted rights from the
 *   Stationers' Company to individual authors, or whether it created a new
 *   conceptual space. The two are related but analytically distinct. This
 *   constraint story models the statute as generative of a new concept; the
 *   sibling readings address institutional reallocation and the
 *   inseparability of the two dimensions.
 *
 * KEY AGENTS:
 *   - Parliament licensing authority (agenda_setter, institutional power)
 *   - Public learning commons (beneficiary, organized)
 *   - Author category (beneficiary, moderate power, emergent identity)
 *   - Stationers' Company monopoly (excluded, previously powerful, now conceptually marginalized)
 *   - Continental natural-rights theorists (observer, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.38).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.22).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Copyright as Limited Regulatory Tool for Learning (Conceptual Emergence Reading)").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'a718676d-121b-459e-8ecb-22f196fa569e').
narrative_ontology:cs_kernel_codification('a718676d-121b-459e-8ecb-22f196fa569e', fixed_text).
narrative_ontology:cs_authority_grounding('a718676d-121b-459e-8ecb-22f196fa569e', lineage).
narrative_ontology:cs_interpretation_layer_present('a718676d-121b-459e-8ecb-22f196fa569e').
narrative_ontology:cs_reading_relation('a718676d-121b-459e-8ecb-22f196fa569e', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('a718676d-121b-459e-8ecb-22f196fa569e', statute_of_anne_ip_foundation__entangled_event_reading, forecloses).
narrative_ontology:cs_axiom('a718676d-121b-459e-8ecb-22f196fa569e', foundational, copyright_is_statutory_grant_not_natural_right).
narrative_ontology:cs_axiom_status(copyright_is_statutory_grant_not_natural_right, holdable).
narrative_ontology:cs_axiom_grounding('a718676d-121b-459e-8ecb-22f196fa569e', copyright_is_statutory_grant_not_natural_right, conventional).
narrative_ontology:cs_axiom('a718676d-121b-459e-8ecb-22f196fa569e', foundational, copyright_term_limit_is_essential_to_category).
narrative_ontology:cs_axiom_status(copyright_term_limit_is_essential_to_category, holdable).
narrative_ontology:cs_axiom_grounding('a718676d-121b-459e-8ecb-22f196fa569e', copyright_term_limit_is_essential_to_category, conventional).
narrative_ontology:cs_reference_frame('a718676d-121b-459e-8ecb-22f196fa569e', perpetual_monopoly_as_natural_default).
narrative_ontology:cs_drift_state('a718676d-121b-459e-8ecb-22f196fa569e', post_statute_reception_by_1750, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('a718676d-121b-459e-8ecb-22f196fa569e', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning_commons).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, emergent_author_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, author_category).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, ip_as_distinct_legal_category).
narrative_ontology:constraint_vindicates(statute_of_anne_ip_foundation__conceptual_emergence_reading, copyright_as_temporal_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the statute as a regulatory intervention in publishing monopoly. Established a new conceptual frame: copyright is a limited grant of right, not a property perpetual. Their act created the space in which 'IP as distinct category' became thinkable—a new vocabulary emerged where none existed before.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament_licensing_authority, agenda_setter,
    institutional, generational, analytical, national).

% Gains access to published works after copyright term expires—the public domain now has a concept and a legal mechanism. Learns from prior art explicitly permitted; the statute's fourteen-year term creates a known endpoint where knowledge becomes freely available. Before the statute, the conceptual frame did not separate 'limited monopoly' from 'perpetual property'—the statute created that distinction.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning_commons, beneficiary,
    organized, generational, analytical, national).

% Becomes a recognized legal category distinct from the Stationers' Company. The statute names authors as potential rights-holders—a new institutional identity emerges. Before the statute, 'author' had no statutory standing; after, authorship is a legal position. This is a conceptual emergence, not merely a reallocation of pre-existing rights.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, author_category, beneficiary,
    moderate, biographical, constrained, national).

% Previously held perpetual copyright by royal charter. The statute does not formally dismantle the Company, but creates a competing conceptual frame in which perpetual monopoly is no longer the natural or only conceivable form of IP protection. Exclusion is from the new legal vocabulary—the statute makes perpetuity unthinkable as the default IP structure.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_monopoly, excluded,
    powerful, generational, trapped, national).

% Observe from a distance. The statute is an English regulatory move; continental natural-rights frameworks ground IP in author labor and inherent rights, not limited regulatory grant. This reading does not adjudicate that contest—it names what the statute did: created a conceptual space where 'limited term' became a defensible IP frame.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, continental_natural_rights_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__conceptual_emergence_reading, diffuse).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__conceptual_emergence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a recognized legal vocabulary and institutional position for 'limited copyright'—a temporal monopoly that reverts to public domain. Solves the conceptual problem: what vocabulary can distinguish 'temporary monopoly for learning incentive' from 'perpetual property'? The statute's answer: copyright is a statutory grant, not an inherent right, and its term is limited by law, not by owner choice.
% TRANSFER_FUNCTION: Does not transfer resources directly. Instead, transfers a conceptual frame: from 'printing monopoly is perpetual property' to 'copyright is a limited regulatory device, with an expiration date built in'. The public gains future access to works after the term; authors gain named legal standing in exchange for accepting temporal limits.
% ABSENT_VOICES: Stationers' Company advocates, defended by parliamentary petition, did not prevail in shaping the statute's conceptual frame—they could argue for perpetuity, but the statute made that argument structurally outside the new legal vocabulary. Authors in other jurisdictions (continental Europe) operate under different conceptual frames (natural rights, perpetual author's right). The statute marginalizes but does not silence these alternatives.
% DISAPPEARANCE_RATIONALE: The statute's disappearance would not erase the conceptual space it created—the distinction between 'limited regulatory grant' and 'perpetual property' is now available in English law regardless of whether *this statute* remains. But the statute was the historical event that made that space thinkable; remove it and the question 'why did we decide copyright is limited?' loses its original answer. The verdict is contested because one reading (institutional reallocation) claims the statute merely reallocated pre-existing concepts, while this reading claims it generated a new concept.
% FOUNDING_PROBLEM: How to distinguish legitimate monopoly (incentive for authors and publishers to produce learning materials) from illegitimate perpetuity (a rent-extraction mechanism that freezes knowledge under lock)? English licensing law and the Stationers' Company operated without a conceptual frame to make this distinction. The statute provides one: copyright is a statutory regulatory tool with a term limit, not a natural or perpetual property.
% FOUNDING_PROBLEM_CORROBORATION: The statute's preamble itself states the problem: promoting learning while controlling monopoly. Parliamentary debates preceding the statute attest the problem—how to break the Stationers' perpetual monopoly while incentivizing authorship. This reading's corroboration comes from historians of IP (Rose, Ginsburg, Deazley) who analyze the statute as a conceptual innovation, not merely institutional rearrangement. Stationers' Company petitions corroborate the problem by defending perpetuity as the natural state—their defense proves the statute's frame was not inevitable.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, contested).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).

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
 *   Extractiveness starts at 0.85 (pre-statute: perpetual monopoly extraction) and falls to 0.38 by 1750 as the new conceptual frame takes hold. The statute does not eliminate extraction—authors and publishers still control access—but it establishes a known endpoint, shifting extraction from unlimited rent to bounded contribution. Suppression requirement falls sharply from 0.78 to 0.22: before the statute, perpetual monopoly had to be defended by royal charter and guild enforcement; after, the statute's legal codification means the extractive arrangement needs less active suppression (it is now law, not contested custom). Theater ratio stays low (0.08–0.15): the statute is genuinely about learning incentive, not pure performance, so theatrical maintenance is minimal. Accessibility collapse is high (0.72) because once the statute's frame is accepted, perpetual monopoly becomes legally unthinkable—alternatives (public domain access, author as legal category) are now in the vocabulary. Resistance is moderate (0.35) because the Stationers' Company and continental theorists do mount a conceptual counter, but the statute's frame is codified in law and Parliament's authority is institutional.
 *
 * PERSPECTIVAL GAP:
 *   The payer/beneficiary structure in this reading is not hierarchical—the statute establishes coordination among Parliament (legal authority), authors (now-recognized category), and public learning (now-explicit goal). The Stationers' Company is not a payer; they are excluded. Perpetual monopoly is eliminated from the legal vocabulary entirely. This differs from an extractive constraint where one seat collects rents and another pays; here, the statute creates a new institutional position (author) and a new legal endpoint (public domain), neither of which existed before. The conceptual emergence is the constraint, not resource transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   Public learning commons benefits from the new vocabulary: 'public domain' becomes a legal concept, accessible after term expiration. Authors benefit from legal recognition (named as potential rights-holders, emergent legal category). The constraint does not extract from these beneficiaries—it coordinates their interests with Parliament's interest in incentivizing publication. The Stationers' Company is NOT a beneficiary; it is excluded from the new frame. Perpetual monopoly is not a stakeholder; it is a now-illegitimate institutional practice. Directionality is near 0.0 for public learning commons and the author category (they benefit without being forced to sustain extraction), and undefined for the Stationers' Company (they are excluded, not coordinated). Parliament's d is analytical (agenda-setter, not extracting or benefiting).
 *
 * MANDATROPHY ANALYSIS:
 *   The statute's mandate is 'promote learning by granting limited copyright to authors.' This mandate is live: 250+ years of English and subsequently Anglo-American IP law operates from this frame. The statute's founding problem (how to escape perpetual monopoly while incentivizing authorship) remains structurally present, though partly solved. Mandatrophy is not present because the statute's function (create a vocabulary for bounded copyright) persists. The institutional reallocation (from Stationers' Company to authors) might atrophy—authors as a category could fade if publishers become the de facto perpetual copyright holders—but the conceptual innovation (copyright is bounded, not perpetual, and returns to the public) is codified in the statute's text and continues to shape legal reasoning. Mandatrophy would arise if, centuries later, copyright were extended indefinitely and the statute's original frame were explicitly repudiated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_emergence_vs_institutional_change,
    'Is the statute''s primary innovation conceptual (it created a new legal vocabulary for ''limited copyright'') or institutional (it reallocated rights from the Stationers'' Company to authors)? Can these be separated analytically?',
    'Textual analysis of the statute''s preamble, legislative debates, and early interpretation. Trace whether advocates for the statute explicitly articulated a new concept (''copyright as bounded'') or merely proposed institutional rearrangement. Examine early IP doctrine to see whether the statute was cited for conceptual innovation or institutional reallocation.',
    'If primarily conceptual, the statute is a boundary-establishing innovation that generated new legal categories. If primarily institutional, it is a redistribution mechanism. If inseparable, both readings collapse into one and no single analytical frame suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_emergence_vs_institutional_change, conceptual, 'Whether the statute''s main effect was conceptual innovation or institutional reallocation, and whether these can be separated.').

omega_variable(
    natural_law_vs_statutory_grant_frame,
    'Does the statute''s frame (''copyright is a statutory regulatory grant, not a natural right'') establish a conceptual foundation for all subsequent English IP law, or is it merely one option among competing natural-rights and common-law views?',
    'Trace IP jurisprudence from Donaldson v. Becket forward. Determine whether courts cite the statute as establishing a regulatory model or whether they appeal to natural-rights/common-law reasoning that brackets or reframes the statute''s explicit claim. Examine how continental natural-rights theory influenced English law despite the statute.',
    'If the statute''s frame is foundational, it generated a persistent conceptual space. If courts and later legislators adopted competing frames (natural rights, perpetual author''s right), the statute''s conceptual innovation was partial or contested. This affects whether the constraint is the statute''s frame or the ongoing contest between statutory and natural-rights vocabularies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_statutory_grant_frame, empirical, 'Whether the statute''s statutory-grant frame dominated English IP law or was persistently contested by natural-rights and common-law alternatives.').

omega_variable(
    public_domain_as_concept,
    'Before the statute, did English law have a concept of ''public domain'' (works free to use after monopoly expires), or did the statute create that concept by implying a reversion to non-protected status after term expiration?',
    'Examine pre-statute legal and publishing practice. Determine whether perpetual monopoly was justified as natural or merely customary. Trace post-statute usage of ''public domain'' language to see if it emerges from the statute''s term-limit structure or from independent natural-law reasoning about common knowledge.',
    'If the statute created the concept, it was genuinely innovative. If ''public domain'' as a concept existed independently, the statute merely codified it. This affects whether the statute is emergent or consolidating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_domain_as_concept, empirical, 'Whether the statute created the concept of public domain or merely codified it.').

omega_variable(
    kernel_reading_multiplicity,
    'The statute is being read here as a kernel with at least three distinct readings (conceptual_emergence, institutional_reallocation, entangled_event). Can additional readings be generated from the same kernel that are also defensible?',
    'Consider alternative framings: (1) The statute as a technological adaptation (printing press created new distribution problems requiring new regulation). (2) The statute as a compromise between conflicting interests (authors wanted rights, the public wanted access, Parliament found a middle ground). (3) The statute as a failed attempt to establish perpetuity under a new name (copyright was supposed to replace the Stationers'' monopoly but became a competing monopoly).',
    'If only three readings are defensible, the kernel''s contest space is bounded. If more readings are available, the kernel is more under-determined. This affects whether any single reading can claim to capture the statute''s essential meaning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Whether the statute admits more than three structurally distinct readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 1660, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1660, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1660, 0.15).
narrative_ontology:measurement_basis(stat_tr_t1660, projected).
narrative_ontology:measurement(stat_tr_t1690, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1690, 0.12).
narrative_ontology:measurement_basis(stat_tr_t1690, projected).
narrative_ontology:measurement(stat_tr_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1710, 0.09).
narrative_ontology:measurement_basis(stat_tr_t1710, observed).
narrative_ontology:measurement(stat_tr_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1730, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1730, observed).
narrative_ontology:measurement(stat_tr_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement_basis(stat_tr_t1750, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1660, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1660, 0.85).
narrative_ontology:measurement_basis(stat_be_t1660, projected).
narrative_ontology:measurement(stat_be_t1690, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1690, 0.72).
narrative_ontology:measurement_basis(stat_be_t1690, projected).
narrative_ontology:measurement(stat_be_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1710, 0.48).
narrative_ontology:measurement_basis(stat_be_t1710, observed).
narrative_ontology:measurement(stat_be_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1730, 0.41).
narrative_ontology:measurement_basis(stat_be_t1730, observed).
narrative_ontology:measurement(stat_be_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement_basis(stat_be_t1750, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1660, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1660, 0.78).
narrative_ontology:measurement_basis(stat_su_t1660, projected).
narrative_ontology:measurement(stat_su_t1690, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1690, 0.65).
narrative_ontology:measurement_basis(stat_su_t1690, projected).
narrative_ontology:measurement(stat_su_t1710, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1710, 0.35).
narrative_ontology:measurement_basis(stat_su_t1710, observed).
narrative_ontology:measurement(stat_su_t1730, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1730, 0.26).
narrative_ontology:measurement_basis(stat_su_t1730, observed).
narrative_ontology:measurement(stat_su_t1750, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 1750, 0.22).
narrative_ontology:measurement_basis(stat_su_t1750, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% The Statute of Anne kernel admits three structurally distinct constraint stories. This reading (conceptual_emergence_reading) isolates the statute as a generator of a new legal vocabulary ('copyright as bounded regulatory tool'). Sibling readings address institutional reallocation and the inseparability thesis. All three share the same historical artifact (the statute) but analyze different structural questions. The conceptual reading influences the institutional reading (if a new concept was created, institutions adopted it; if not, institutions were merely rearranged), and both are influenced by the entangled reading's claim that separation is false. Constraint family members must be linked for the system to model the contest across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
