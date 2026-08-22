% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Statute of Anne (1710): Author Entry into the Legitimate Claimant Set for Copy-Right
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the first_holding_reading of the
 *   ip_category_emergence kernel: the 1710 Statute of Anne is read here
 *   specifically as a shift in the membership of the legitimate claimant set
 *   for copy — the author enters that set for the first time as a
 *   term-limited statutory holder, displacing the Stationers' Company's prior
 *   claim to perpetual guild-enforced copy. This is a distinct structural
 *   claim from asking whether 'ownable expression' became a newly coherent
 *   legal category at that moment (the thinkability_reading) — occupancy of
 *   an existing enforcement slot is a different fact from the conceptual
 *   availability of the category the slot belongs to. The two readings could
 *   in principle be extensionally identical (the moment of category-emergence
 *   and the moment of occupancy-shift might coincide exactly) or could pull
 *   apart (occupancy could shift within an already-thinkable category, or the
 *   category could become thinkable without immediately being occupied by a
 *   new claimant) — that possible collapse is exactly what the
 *   synchronic_diachronic_seam reading interrogates, and this story
 *   deliberately does not resolve it, per Rule 1's discipline of authoring
 *   one clean reading.
 *
 * KEY AGENTS:
 *   - statutory_authors: newly named first holder, biographical horizon, constrained exit — mostly assigns the right away immediately
 *   - publisher_assignees: prior de facto holder under guild custom, now the practical assignee-holder under the new statute — organized, mobile, retains commercial position
 *   - stationers_company_incumbents: loses a perpetual claim, retains only what it can convert into statutory assignee positions — organized but constrained, litigates for decades afterward
 *   - the_public_domain_at_large: bears the exclusivity during the term regardless of who holds it, benefits only at term expiration — powerless, trapped, civilizational horizon
 *   - crown_and_parliament: agenda-setter who fixes both the new claimant and the term length
 *   - legal_historians: analytical observer separating the occupancy question from the category-emergence question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.42).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.38).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statute of Anne (1710): Author Entry into the Legitimate Claimant Set for Copy-Right").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '15f36fe3-b611-4a10-af8d-608cf038cc50').
narrative_ontology:cs_kernel_codification('15f36fe3-b611-4a10-af8d-608cf038cc50', formalized).
narrative_ontology:cs_authority_grounding('15f36fe3-b611-4a10-af8d-608cf038cc50', lineage).
narrative_ontology:cs_interpretation_layer_present('15f36fe3-b611-4a10-af8d-608cf038cc50').
narrative_ontology:cs_reading_relation('15f36fe3-b611-4a10-af8d-608cf038cc50', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('15f36fe3-b611-4a10-af8d-608cf038cc50', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('15f36fe3-b611-4a10-af8d-608cf038cc50', foundational, author_named_first_statutory_holder).
narrative_ontology:cs_axiom_status(author_named_first_statutory_holder, holdable).
narrative_ontology:cs_axiom_grounding('15f36fe3-b611-4a10-af8d-608cf038cc50', author_named_first_statutory_holder, conventional).
narrative_ontology:cs_axiom('15f36fe3-b611-4a10-af8d-608cf038cc50', secondary, occupancy_shift_independent_of_category_coherence).
narrative_ontology:cs_axiom_status(occupancy_shift_independent_of_category_coherence, holdable).
narrative_ontology:cs_axiom_grounding('15f36fe3-b611-4a10-af8d-608cf038cc50', occupancy_shift_independent_of_category_coherence, conventional).
narrative_ontology:cs_reference_frame('15f36fe3-b611-4a10-af8d-608cf038cc50', stationers_perpetual_guild_entitlement).
narrative_ontology:cs_drift_state('15f36fe3-b611-4a10-af8d-608cf038cc50', post_statute_of_anne_enactment, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('15f36fe3-b611-4a10-af8d-608cf038cc50', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, publisher_assignees).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company_incumbents).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, the_public_domain_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, the_public_domain_at_large).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, authorial_labor_desert_doctrine).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, statutory_grant_supremacy_over_guild_custom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prior to 1710 had no independent statutory footing to claim copy-right; their work was governed entirely by the perpetual entry rights the Stationers' Company held in the Stationers' Register. The 1710 Act names the author as an original holder of a term-limited right (14 years, renewable once), for the first time placing them inside the set of parties whose claim the state will enforce. Authors gain a bargaining chip they can sell or license to a bookseller, but most immediately assign it away for a lump sum, so occupancy of the set does not translate into ongoing control for most working authors.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% The London booksellers who previously held perpetual copy under guild custom quickly absorb the new statutory mechanism: they purchase the term-limited author right at the point of publication and continue to function as the practical rights-holder. They lobbied for and helped draft the Act's enforcement provisions and benefit from a legal category that looks like reform but preserves their commercial position by contract, so their exit options remain wide even as the formal claimant set changes beneath them.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, publisher_assignees, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, publisher_assignees, agenda_setter).

% Held perpetual, guild-enforced copy under custom and prior licensing acts; the 1710 statute terminates that perpetual claim and substitutes a term-limited statutory right vested first in the author. Some incumbents adapt by becoming assignees under the new regime; those who cannot pivot lose their prior exclusive footing entirely when their existing stock's terms lapse. They resisted the bill in Parliament and continued to litigate a claim to perpetual common-law copyright for decades afterward (culminating in Donaldson v. Beckett, 1774).
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company_incumbents, payer,
    organized, generational, constrained, national).

% Readers, subsequent authors, and printers of expired works are structurally implicated by where the term boundary is drawn: a term-limited author right (versus the guild's asserted perpetual claim) eventually returns works to unrestricted use, which is a benefit relative to perpetual guild control, but during the statutory term the public bears the same exclusionary restriction the Stationers previously imposed, now relabeled as an author's right rather than a bookseller's monopoly. This population is not present in the parliamentary bargaining that fixed the term.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, the_public_domain_at_large, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, the_public_domain_at_large, beneficiary).

% Drafts and enacts the statute, choosing to name the author (not the guild) as the first legitimate holder and fixing the term length. Motivated partly by a desire to break the Stationers' perpetual licensing monopoly (associated with censorship-era press control) and partly by continued pressure from the book trade for some enforceable exclusivity. Administers the registration and enforcement apparatus that makes the new claimant set operative rather than merely declaratory.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, crown_and_parliament, agenda_setter,
    institutional, generational, analytical, national).

% Study the 1710 Act as the moment the legitimate claimant set for copy shifted membership — from guild-holder-only to author-as-first-holder — independent of the separate question of whether the underlying category of ownable expression was newly thinkable at that moment. This reading treats the occupancy shift and the conceptual-emergence question as analytically separable claims about the same historical event.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, publisher_assignees).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single statutory mechanism for resolving conflicting claims to control the copying of a printed work, replacing an ad hoc guild-registry system with a term-limited, nationally enforceable right — solving the real coordination problem of who may sue whom over unauthorized copying.
% TRANSFER_FUNCTION: Moves the position of 'first legitimate claimant' from the Stationers' Company (via perpetual guild entry) to the individual author (via a statutory term), and in the same motion moves the practical economic benefit of that claimant position from author to publisher-assignee through near-universal contractual assignment at publication.
% ABSENT_VOICES: Readers, subsequent adapting authors, and the un-consulted public whose access is bounded by the term are not parties to the parliamentary negotiation, which is conducted between the book trade (seeking exclusivity) and reform-minded legislators (seeking to end the licensing-censorship apparatus); their interests are represented only derivatively through the term-limit compromise.
% DISAPPEARANCE_RATIONALE: If the 1710 membership shift were reversed — if the author had never been named a legitimate first holder — the entire subsequent architecture of authorial copyright (assignment, royalty contracts, moral rights debates, the author-function in law) would rest on a different foundation; publishers would instead hold direct, unmediated guild-style claims, and centuries of doctrine treating the author as the originating rights-bearer would have no statutory root to trace to.
% FOUNDING_PROBLEM: The Licensing of the Press Act had lapsed in 1695, leaving no statutory basis for controlling unauthorized reprinting; the Stationers' Company sought a replacement exclusivity mechanism, and Parliament sought one that would not reconstitute a censorship-enabling perpetual monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Stationers' Company petitions and subsequent litigation (Donaldson v. Beckett, 1774) attest the trade believed the founding problem was 'restore our exclusivity,' now resolved against them; legal historians outside both the trade and the authorial class (e.g. modern doctrinal historians of the Statute of Anne) attest the problem Parliament actually solved was narrower — ending licensing-era perpetual guild control — and that the author-as-first-holder framing was a instrumentally chosen mechanism for that narrower end, not a considered judgment about authorial desert.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).
:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42-0.45 because the underlying exclusivity mechanism (a party may exclude others from copying for a term) persists across the 1710 shift — what changes is WHO occupies the enforceable claimant position, not whether an exclusionary mechanism exists at all. This is why extractiveness dips slightly at 1710 (0.4) rather than spiking: the immediate legal event is a reallocation of standing, and the practical extraction pattern (publishers realizing commercial benefit) reasserts itself over the following decades as assignment contracts normalize. Suppression drops sharply at 1710 (from 0.55 perpetual-guild-era to 0.3) because a term-limited right is structurally less suppressive than the perpetual claim it replaces, then creeps back upward through 1774 as publishers and Parliament layer renewal practices and litigation-tested enforcement onto the statutory floor. Theater ratio rises slowly and modestly, reflecting the growing gap between the author-protection rationale publicly offered for the Act and the practical publisher-capture of its benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Statutory authors are declared beneficiaries because the Act's textual grant runs to them first, but their directionality should not be read as strongly beneficiary in practice — most immediately assign the right, so the formal occupancy gain does not track a durable economic benefit; this is a case where declared beneficiary status and lived structural position diverge, and the story's beneficiary/victim declarations track the FORMAL legal occupancy the first_holding_reading is specifically about, not the ultimate economic flow (which the engine's directionality computation should be read alongside the commentary here, not instead of it). Publisher-assignees are the more stable beneficiary in practical terms, which is why they carry organized power and mobile exit despite not being the named statutory holder. Stationers' Company incumbents are the clearest victims of the occupancy shift itself: their perpetual claim is directly terminated by the same statute that creates the author's claim. The public domain is a victim on the timescale of the term and a long-run beneficiary at its expiration — both roles are declared to capture this asymmetric temporal structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending Licensing Act-era perpetual censorship-enabling guild control) is largely resolved by 1710 itself — the perpetual Stationers' claim is terminated in the same act that creates the term-limited author claim. What persists afterward (Stationers' litigation through 1774, publisher assignment practice, gradually rising theater ratio around 'author protection' framing) is not the original founding problem but a layered set of subsequent extractive adaptations riding on the new statutory form. This is why founding_problem_status is authored as contested rather than flatly dead: the trade continued for six decades to assert that a different problem (protecting exclusivity as such) remained live, while the statute's actual drafters had solved a narrower problem and moved on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupancy_shift_vs_category_emergence_independence,
    'Is the 1710 shift in who occupies the legitimate-claimant slot (this reading) a structurally separate fact from whether the category of ownable expression became newly thinkable at the same moment (the thinkability_reading), or are these the same underlying event described at two different levels of description?',
    'The synchronic_diachronic_seam reading is built specifically to test this via an M4/M5 collapse analysis: if occupancy-shift and category-emergence covary perfectly across every historical instance where either could be tested (other jurisdictions'' copyright statutes, common-law copyright claims pre- and post-statute), the two readings collapse into one constraint; if they can be shown to vary independently (e.g., an occupancy shift without new category-thinkability, or vice versa), they remain genuinely distinct.',
    'If the readings collapse, this story''s ε and the thinkability_reading''s ε should in principle converge, and maintaining them as separate constraints would violate the ε-invariance principle; if they remain independent, the two stories correctly model distinct structural claims and should stay separate with the network edge documenting the relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupancy_shift_vs_category_emergence_independence, conceptual, 'Whether the first-holding and thinkability readings describe one event or two structurally independent facts.').

omega_variable(
    formal_occupancy_vs_economic_capture,
    'Should the beneficiary declaration for statutory_authors reflect their formal legal position as first holder, or their practical economic position after near-universal contractual assignment to publishers?',
    'Historical contract records from the period (assignment deeds, bookseller ledgers) could establish what fraction of authors retained versus assigned their statutory right, and on what terms, clarifying whether ''benefit'' should be read formally or economically for this population.',
    'If economic capture by publishers is treated as the operative fact, the beneficiary declaration should shift toward publisher_assignees alone, and the constraint''s structure looks more like a snare wearing an author-protection rope''s clothing; if formal occupancy is treated as the operative fact (consistent with this reading''s stated focus), the beneficiary declaration for statutory_authors stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_occupancy_vs_economic_capture, conceptual, 'Whether beneficiary status tracks formal legal occupancy or downstream economic capture.').

omega_variable(
    guild_custom_as_prior_natural_baseline,
    'Was the Stationers'' Company''s perpetual guild claim itself a natural or customary baseline being displaced by statute, or was it already an artificial extraction resting on licensing-act coercion that the 1710 statute merely replaced with a different artificial arrangement?',
    'Comparative analysis of guild registry practice under the lapsed Licensing Acts versus common-law claims asserted independent of licensing — if guild control depended entirely on licensing-act enforcement machinery, its ''naturalness'' as a prior baseline is undermined.',
    'If guild custom was itself artificial and coercively maintained, then framing the 1710 shift as a victimization of Stationers'' incumbents somewhat overstates their prior legitimate claim; if guild custom had independent common-law standing, the victim declaration is more clearly warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guild_custom_as_prior_natural_baseline, conceptual, 'Whether the pre-1710 guild claim being displaced was itself a natural baseline or a prior artificial extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1690, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__first_holding_reading, theater_ratio, 1690, 0.1).
narrative_ontology:measurement_basis(ip_c_tr_t1690, observed).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement_basis(ip_c_tr_t1710, observed).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.18).
narrative_ontology:measurement_basis(ip_c_tr_t1725, observed).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__first_holding_reading, theater_ratio, 1740, 0.2).
narrative_ontology:measurement_basis(ip_c_tr_t1740, observed).
narrative_ontology:measurement(ip_c_tr_t1758, ip_category_emergence__first_holding_reading, theater_ratio, 1758, 0.21).
narrative_ontology:measurement_basis(ip_c_tr_t1758, observed).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__first_holding_reading, theater_ratio, 1774, 0.22).
narrative_ontology:measurement_basis(ip_c_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__first_holding_reading, base_extractiveness, 1690, 0.5).
narrative_ontology:measurement_basis(ip_c_be_t1690, observed).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.4).
narrative_ontology:measurement_basis(ip_c_be_t1710, observed).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.43).
narrative_ontology:measurement_basis(ip_c_be_t1725, observed).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__first_holding_reading, base_extractiveness, 1740, 0.44).
narrative_ontology:measurement_basis(ip_c_be_t1740, observed).
narrative_ontology:measurement(ip_c_be_t1758, ip_category_emergence__first_holding_reading, base_extractiveness, 1758, 0.45).
narrative_ontology:measurement_basis(ip_c_be_t1758, observed).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__first_holding_reading, base_extractiveness, 1774, 0.42).
narrative_ontology:measurement_basis(ip_c_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1690, ip_category_emergence__first_holding_reading, suppression_requirement, 1690, 0.55).
narrative_ontology:measurement_basis(ip_c_su_t1690, observed).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.3).
narrative_ontology:measurement_basis(ip_c_su_t1710, observed).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.32).
narrative_ontology:measurement_basis(ip_c_su_t1725, observed).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__first_holding_reading, suppression_requirement, 1740, 0.34).
narrative_ontology:measurement_basis(ip_c_su_t1740, observed).
narrative_ontology:measurement(ip_c_su_t1758, ip_category_emergence__first_holding_reading, suppression_requirement, 1758, 0.36).
narrative_ontology:measurement_basis(ip_c_su_t1758, observed).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__first_holding_reading, suppression_requirement, 1774, 0.38).
narrative_ontology:measurement_basis(ip_c_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint is the first_holding_reading member of the ip_category_emergence kernel family. The thinkability_reading addresses whether ownable expression became a coherent legal category in 1710 (a conceptual-emergence claim); this story addresses who occupies the legitimate-claimant slot once such a category exists or is presumed (an occupancy-shift claim). The synchronic_diachronic_seam constraint tests whether these two claims are formally independent or collapse into a single event under M4/M5 analysis. All three share the historical event (the Statute of Anne, 1710) but author distinct ε values and distinct beneficiary/victim structures because they are structurally distinct claims about that event, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
