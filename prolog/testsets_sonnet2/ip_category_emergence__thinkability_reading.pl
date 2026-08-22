% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: Emergence of 'Ownable Expression' as a Legally Coherent Category (Statute of Anne, 1710)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story authors the THINKABILITY reading of the IP category emergence
 *   kernel: the claim that 1710 is significant because it is the moment
 *   'ownable expression' becomes a coherent legal category at all — a new
 *   conceptual slot in the space of legally thinkable claims — distinct from
 *   the sibling claim (first_holding_reading) that 1710 is significant
 *   because a new class of claimant (the author) enters an already-thinkable
 *   category. This reading's referent is the standing arrangement under
 *   contest: the doctrinal category itself, as it operates once thinkable,
 *   not the ideal rights-respecting regime it might have produced. Pre-1710,
 *   disputes over reprinting were litigated (where litigated at all) through
 *   guild entry-book custom and stationers' privilege, a vocabulary that had
 *   no purchase outside the London trade and no conceptual contrast class of
 *   'the public domain.' The Statute of Anne's deployment of 'copy right' as
 *   a term of art distinct from guild privilege is the event that makes a
 *   whole family of subsequent claims sayable: infringement of an intangible
 *   object, terms of protection, works falling into public use. The
 *   category's coherence is itself a resource, and the story traces who was
 *   positioned to exploit the new conceptual space (the incumbent trade,
 *   re-describing its old privilege in the new vocabulary) versus who was
 *   structurally unable to enter it (oral tradition, which has no fixed,
 *   attributable, individuated work to plug into the category).
 *
 * KEY AGENTS:
 *   - named_authors: primary intended beneficiary of the new category (moderate/constrained) — gains a conceptual slot but rarely retains the right in practice
 *   - london_stationers_guild_successors: incumbent trade re-describing old privilege in new vocabulary (organized/arbitrage) — captures much of the conceptual space
 *   - provincial_printers: bear new nationally-coherent liability where local ambiguity once protected them (moderate/constrained)
 *   - oral_and_folk_tradition_bearers: structurally excluded from the category itself, not merely disadvantaged within it (powerless/trapped)
 *   - public_domain_claimants: the category's shadow-class, contested for decades (powerless/trapped)
 *   - legal_historians: analytical observers distinguishing the thinkability question from the occupancy question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.58).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.62).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Emergence of 'Ownable Expression' as a Legally Coherent Category (Statute of Anne, 1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'c4707e10-ef69-4626-8046-2f67a0d169f4').
narrative_ontology:cs_kernel_codification('c4707e10-ef69-4626-8046-2f67a0d169f4', fixed_text).
narrative_ontology:cs_authority_grounding('c4707e10-ef69-4626-8046-2f67a0d169f4', lineage).
narrative_ontology:cs_interpretation_layer_present('c4707e10-ef69-4626-8046-2f67a0d169f4').
narrative_ontology:cs_reading_relation('c4707e10-ef69-4626-8046-2f67a0d169f4', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_reading_relation('c4707e10-ef69-4626-8046-2f67a0d169f4', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('c4707e10-ef69-4626-8046-2f67a0d169f4', foundational, expression_is_a_distinct_ownable_object_independent_of_privilege).
narrative_ontology:cs_axiom_status(expression_is_a_distinct_ownable_object_independent_of_privilege, holdable).
narrative_ontology:cs_axiom_grounding('c4707e10-ef69-4626-8046-2f67a0d169f4', expression_is_a_distinct_ownable_object_independent_of_privilege, conventional).
narrative_ontology:cs_axiom('c4707e10-ef69-4626-8046-2f67a0d169f4', secondary, category_coherence_precedes_and_enables_claimant_legitimacy).
narrative_ontology:cs_axiom_status(category_coherence_precedes_and_enables_claimant_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c4707e10-ef69-4626-8046-2f67a0d169f4', category_coherence_precedes_and_enables_claimant_legitimacy, conventional).
narrative_ontology:cs_reference_frame('c4707e10-ef69-4626-8046-2f67a0d169f4', guild_entry_book_custom).
narrative_ontology:cs_drift_state('c4707e10-ef69-4626-8046-2f67a0d169f4', post_donaldson_v_becket_settlement, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('c4707e10-ef69-4626-8046-2f67a0d169f4', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, london_stationers_guild_successors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, named_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, print_capital_investors).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, provincial_printers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, oral_and_folk_tradition_bearers).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, public_domain_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Before 1710, an author's claim to a text was not a legally cognizable category distinct from the printer's privilege; after the Statute of Anne, authorship itself becomes a locus the law can recognize and vest rights in. This gives named authors a new conceptual slot to occupy, though in practice most still assign the vested right to a bookseller immediately upon publication.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, named_authors, beneficiary,
    moderate, biographical, constrained, national).

% Having lost their perpetual common-law-style entry-based monopoly, the London trade re-describes its commercial interest in the new vocabulary of 'copy right,' successfully translating an old guild privilege into the new legally thinkable category and thereby capturing much of the conceptual space the statute opened.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, london_stationers_guild_successors, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, london_stationers_guild_successors, beneficiary).

% Booksellers and financiers who fund print runs benefit from a newly thinkable category of ownable expression because it gives their capital outlay a transferable, assignable legal object to attach to, independent of the old entry-book system tied to guild membership.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, print_capital_investors, beneficiary,
    powerful, generational, mobile, national).

% Printers outside the London trade, previously operating in gray zones where entry-book custom had only local force, now face a nationally coherent legal category of ownable expression that can be asserted against them in courts they cannot easily reach; the conceptual clarity that helps London claimants works against provincial reprinters who relied on ambiguity.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, provincial_printers, payer,
    moderate, biographical, constrained, regional).

% Communal, oral, and folk forms of expression have no author-function of the kind the new category requires (a fixed, attributable, individuated work) and so are structurally unable to enter the newly thinkable category at all; the category's emergence quietly recodes what counts as expression worth owning, and their forms simply do not qualify.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, oral_and_folk_tradition_bearers, excluded,
    powerless, civilizational, trapped, local).

% Before the category existed, there was no coherent legal contrast class of 'expired' or 'unownable' expression either — the emergence of ownable expression as thinkable simultaneously creates the shadow category of the public domain, but early practice treats the boundary as contestable and repeatedly re-litigated (as in the Battle of the Booksellers), leaving anyone relying on works falling out of protection without secure footing for decades.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, public_domain_claimants, payer,
    powerless, generational, trapped, national).

% Study the doctrinal and conceptual history of the statute and its aftermath, distinguishing what changed in the space of legally thinkable claims (this reading's object) from what changed in who could actually hold a claim (a separate, sibling question).
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, london_stationers_guild_successors).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, nationally shared vocabulary and doctrinal slot ('copy right' as an incident of authorship) that lets courts, printers, and authors reason about claims to printed expression using a common category, replacing the patchwork of guild entry-book custom that had no force outside the trade.
% TRANSFER_FUNCTION: Moves the power to characterize a dispute — from 'is this printer violating guild custom' to 'is this a copy right in a work' — into a single doctrinal frame; this reallocates argumentative and enforcement leverage toward whoever can most fluently deploy the new category, which in practice was the incumbent London trade re-describing its old privilege.
% ABSENT_VOICES: Oral tradition bearers and provincial printers had no seat in the parliamentary process that produced the statute and no vocabulary of their own with which to contest a category built around fixed, attributable, individuated printed works; their absence is structural, not incidental, since the category itself was built to fit the London trade's existing objects.
% DISAPPEARANCE_RATIONALE: If the conceptual category of ownable expression had never become legally coherent, an enormous amount of subsequent doctrine (moral rights, derivative works, the very idea of infringement as a wrong against an intangible object rather than a guild rule) would have no anchor point; courts would still be forced to reason in terms of entry-book custom or unfair-competition-style analogies, and centuries of downstream categories (patent-copyright distinctions, the public domain itself) would need a different conceptual scaffold entirely.
% FOUNDING_PROBLEM: Parliament needed to dismantle the Stationers' Company's perpetual, guild-based printing monopoly (widely resented as a censorship and price-control mechanism) while still giving the London book trade and authors some legally cognizable claim to prevent piratical reprinting, since a total absence of any claim threatened to collapse investment in new works.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary debate and later legal historians (outside the book trade) attest the founding problem was genuinely about breaking monopoly power and limiting terms, not merely rebranding it; but the London trade's own petitions and subsequent litigation (the Battle of the Booksellers, Donaldson v Becket) show the beneficiary group itself treating the new category as continuous with, and a vehicle for restoring, the old perpetual privilege — the corroboration is genuinely split between outside historians and interested parties reading the same event differently.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 pre-statute to a plateau near 0.58-0.60 as the London trade successfully re-colonizes the new category (peaking around the Battle of the Booksellers era, 1740-1760) before Donaldson v Becket (1774) settles the term limit and extraction eases slightly. Suppression sits moderately high throughout (0.5-0.65) because enforcing the category's boundaries — deciding what does and does not count as ownable expression — required active litigation and parliamentary re-intervention across the interval, not a one-time settlement. Theater ratio rises modestly (0.1 to 0.3) as perpetual-copyright arguments increasingly relied on rhetorical appeals to natural authorial right that outran what the statute's text actually granted.
 *
 * DIRECTIONALITY LOGIC:
 *   Named authors are beneficiaries on paper (the category is built to fit their claim) but their d sits closer to symmetric than pure beneficiary because in practice they assign the right immediately to a bookseller — the print capital investors and guild successors are the structural beneficiaries who actually accrue value from the category's coherence. Provincial printers and public domain claimants are targets: the category's new clarity is precisely what can now be asserted against them where ambiguity once shielded them. Oral tradition bearers are not targets in the extraction sense at all — they are excluded from the category's domain of application entirely, which is a different structural position than being a payer within it, though the practical effect (no claim, no recognition) is similarly severe.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (breaking a censorship-adjacent perpetual monopoly) is genuinely contested as live vs. dead: from the vantage of limiting terms and enabling public-domain entry, the problem was substantially solved and the doctrinal category has since been repurposed toward re-establishing durable control (a mandatrophy-adjacent drift, though not full mandatrophy since the term-limiting function does still operate). Reading this as tangled_rope rather than pure snare or pure rope keeps both true simultaneously: the category coordinates a genuine, useful function (a shared vocabulary for the whole legal system to reason about printed expression) while the same vocabulary was actively captured by the incumbent trade to extract value from provincial printers and against the emerging public domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_occupancy_independence,
    'Is the emergence of a legally thinkable category of ownable expression (this reading) a structurally separate event from the emergence of the author as a legitimate claimant within that category (first_holding_reading), or are these the same historical event described at two different levels of abstraction?',
    'Trace whether any pre-1710 legal argument deployed a category-like vocabulary (''copy,'' ''privilege of printing'') without any coherent claimant theory, and whether any post-1710 argument deployed a claimant theory (author-as-rights-holder) without relying on the newly coherent category — independent variation in either direction would support structural separability; if the two always co-occur and never vary independently across the documentary record, they may be one event under two descriptions, which is precisely the question the synchronic_diachronic_seam story is built to test.',
    'If thinkability and first-holding are independent, this story''s ε and classification stand alone as measuring a distinct structural event; if they collapse into one event under the seam test, this story and first_holding_reading would need to be merged or one would need to be recognized as derivative of the other, which would change how the constraint family''s network edges should be read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thinkability_vs_occupancy_independence, conceptual, 'Whether the thinkability reading and the first-holding reading name one event or two.').

omega_variable(
    guild_privilege_recharacterization_completeness,
    'Did ''copy right'' as deployed in and after 1710 constitute a genuinely new legal category, or was it substantially the old stationers'' entry-book privilege wearing new vocabulary, such that the ''category emergence'' framing overstates the discontinuity?',
    'Compare the substantive content of rights asserted under entry-book custom (pre-1710) against rights asserted under statutory copy right (post-1710) for structural identity or difference in scope, duration, and enforceability, independent of the vocabulary used to describe them.',
    'If the substance is largely continuous with prior guild privilege, this reading''s core claim (a genuinely new category became legally coherent) is weaker than claimed and the constraint may be better read as a tangled_rope leaning toward relabeled snare (old extraction, new name) rather than genuine category emergence with mixed effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guild_privilege_recharacterization_completeness, empirical, 'Whether the 1710 category is substantively novel or a relabeling of prior guild privilege.').

omega_variable(
    cs_framing_kernel_vs_authority_layer,
    'Should the commitment-system kernel here be read as the statutory text of the Statute of Anne itself, or as the deeper legitimacy claim (''expression can be owned independent of its physical medium'') that the statute''s interpreters have layered above the text and that later courts (Millar v Taylor, Donaldson v Becket) actually litigated?',
    'Examine whether courts after 1710 treat the statutory text as self-interpreting or whether they construct and argue over an unwritten background principle (natural authorial property) that the text is read to either confirm or foreclose.',
    'If the deeper legitimacy claim is the true kernel, the authority_grounding is better read as lineage/practice (accreting case-law tradition) rather than a pure fixed_text formalized reading of the 1710 statute; this would shift interpretation_layer_present considerations for this story and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_layer, conceptual, 'Whether the operative kernel is the statutory text or an unwritten background legitimacy principle courts read into it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1690, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__thinkability_reading, theater_ratio, 1690, 0.1).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.15).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__thinkability_reading, theater_ratio, 1725, 0.22).
narrative_ontology:measurement(ip_c_tr_t1740, ip_category_emergence__thinkability_reading, theater_ratio, 1740, 0.28).
narrative_ontology:measurement(ip_c_tr_t1760, ip_category_emergence__thinkability_reading, theater_ratio, 1760, 0.3).
narrative_ontology:measurement(ip_c_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.28).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__thinkability_reading, base_extractiveness, 1690, 0.35).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.42).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__thinkability_reading, base_extractiveness, 1725, 0.5).
narrative_ontology:measurement(ip_c_be_t1740, ip_category_emergence__thinkability_reading, base_extractiveness, 1740, 0.55).
narrative_ontology:measurement(ip_c_be_t1760, ip_category_emergence__thinkability_reading, base_extractiveness, 1760, 0.6).
narrative_ontology:measurement(ip_c_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1690, ip_category_emergence__thinkability_reading, suppression_requirement, 1690, 0.55).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.5).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__thinkability_reading, suppression_requirement, 1725, 0.58).
narrative_ontology:measurement(ip_c_su_t1740, ip_category_emergence__thinkability_reading, suppression_requirement, 1740, 0.62).
narrative_ontology:measurement(ip_c_su_t1760, ip_category_emergence__thinkability_reading, suppression_requirement, 1760, 0.65).
narrative_ontology:measurement(ip_c_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial claim 'the Statute of Anne created intellectual property' into three structurally distinct readings of a single kernel (ip_category_emergence): thinkability_reading (this story — a new legal category becomes sayable), first_holding_reading (a new claimant, the author, enters an existing or newly-formed category), and synchronic_diachronic_seam (a formal test of whether the first two readings are independent or collapse under temporal framing). Each reading authors its own ε, beneficiaries, and victims per the ε-invariance principle; they are linked here rather than merged because measuring 'category coherence' and measuring 'claimant legitimacy' are different observables that could yield different ε values if forced into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
