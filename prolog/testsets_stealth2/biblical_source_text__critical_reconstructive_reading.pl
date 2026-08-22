% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Reading of the Biblical Source Text Kernel
 *   domain: religion/academia/linguistics
 *
 * SUMMARY:
 *   This story instantiates the critical-reconstructive reading of the kernel
 *   biblical_source_text: the methodological order in which historical
 *   recovery of the hypothetical original wording is primary, and neither
 *   source-language structure nor target-language meaning may be privileged
 *   until the textual basis has been established. The standing arrangement
 *   under contest - and the epsilon referent for this story - is that regime
 *   as it actually operates: critical editions maintained by specialist
 *   committees, seminary curricula that require textual criticism before
 *   exegesis, and translation projects that must declare and justify their
 *   base text. The regime solves a real problem (tens of thousands of variant
 *   readings, no principled selection rule) and imposes real asymmetric costs
 *   (communities whose received wording is demoted to 'witness' status absorb
 *   destabilization they did not choose). The claim/metric split is
 *   deliberate: the claimed type is what I judge structurally true of the
 *   arrangement; the metrics describe its observed operation independently.
 *   KEY AGENTS (by structural relationship): - critical_edition_committees:
 *   agenda-setter (institutional/arbitrage) - convenes editorial bodies,
 *   fixes method, prints the text others must use -
 *   academic_biblical_scholarship: primary beneficiary
 *   (organized/identity_locked) - careers, citation capital, and endowed
 *   posts flow through apparatus-mediated access -
 *   mainline_translation_societies: enforcing administrator
 *   (institutional/constrained) - adopts critical base texts as translation
 *   standards - confessional_received_text_communities: primary payer
 *   (organized/identity_locked) - received wording is doctrinally
 *   load-bearing; destabilization strikes at confession -
 *   traditionalist_clergy: payer (moderate/identity_locked) - preaching and
 *   liturgy built on familiar verse forms - lay_congregations: diffuse payer
 *   with incidental gains (moderate/constrained) - encounters the regime only
 *   as footnotes and altered verses - mission_translation_agencies: payer
 *   with coordination gains (organized/constrained) - must train translators
 *   in criticism to use the shared base - majority_world_churches: excluded
 *   voice (organized/constrained) - absorbs decisions made elsewhere -
 *   religious_studies_observers: analytical seat (institutional/analytical) -
 *   uses the texts, takes no confessional side.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Reading of the Biblical Source Text Kernel").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religion/academia/linguistics").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '1f84067e-7b7d-465a-a728-ba2942cd16df').
narrative_ontology:cs_kernel_codification('1f84067e-7b7d-465a-a728-ba2942cd16df', formalized).
narrative_ontology:cs_authority_grounding('1f84067e-7b7d-465a-a728-ba2942cd16df', expertise).
narrative_ontology:cs_interpretation_layer_present('1f84067e-7b7d-465a-a728-ba2942cd16df').
narrative_ontology:cs_reading_relation('1f84067e-7b7d-465a-a728-ba2942cd16df', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('1f84067e-7b7d-465a-a728-ba2942cd16df', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('1f84067e-7b7d-465a-a728-ba2942cd16df', foundational, textual_basis_precedes_privilege_sequence).
narrative_ontology:cs_axiom_status(textual_basis_precedes_privilege_sequence, holdable).
narrative_ontology:cs_axiom_grounding('1f84067e-7b7d-465a-a728-ba2942cd16df', textual_basis_precedes_privilege_sequence, instrumental).
narrative_ontology:cs_axiom('1f84067e-7b7d-465a-a728-ba2942cd16df', foundational, autograph_recovery_is_primary_task).
narrative_ontology:cs_axiom_status(autograph_recovery_is_primary_task, holdable).
narrative_ontology:cs_axiom_grounding('1f84067e-7b7d-465a-a728-ba2942cd16df', autograph_recovery_is_primary_task, empirically_contingent).
narrative_ontology:cs_reference_frame('1f84067e-7b7d-465a-a728-ba2942cd16df', provisional_reconstruction_toward_lost_autographs).
narrative_ontology:cs_drift_state('1f84067e-7b7d-465a-a728-ba2942cd16df', contemporary_post_critical_turn, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f84067e-7b7d-465a-a728-ba2942cd16df', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_edition_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, mainline_translation_societies).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_received_text_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_congregations).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, mission_translation_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, lay_congregations).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, mission_translation_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the editorial bodies that maintain the scholarly editions of the Hebrew and Greek scriptures, fix the criteria by which variant readings are weighed, and print the text that translation projects worldwide adopt as their base. They can revise method between editions and shift the standard; their determinations cascade into every downstream translation and commentary. Prestige and institutional resources concentrate here.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_edition_committees, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, critical_edition_committees, beneficiary).

% Faculty, textual critics, and philologists whose careers, citation networks, and endowed posts depend on apparatus-mediated access to the text. Their entire formation is the critical method; working outside it means leaving the profession. They collect standing, funding, and the authority to adjudicate wording questions, and supply the labor that keeps editions current.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary,
    organized, biographical, identity_locked, global).

% Agencies that adopt critical base texts as their translation standards, train translator teams in their use, and publish the cross-denominational versions most churches read. Enforcing the base-text requirement is how they keep partner denominations at one table; abandoning it would fracture their coalitions and strand completed programs.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, mainline_translation_societies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, mainline_translation_societies, beneficiary).

% Communities whose confessions identify a received wording (a traditional Greek text, a historic vernacular version, a liturgical text) with providentially preserved scripture. Footnotes reporting that the earliest witnesses lack a verse, or editions removing it, strike directly at doctrinal commitments. Adopting the reconstructed text would require repudiating preservation claims that constitute the community's self-understanding, so exit is unthinkable from inside; remaining means absorbing each successive revision as an attack to be answered.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_received_text_communities, payer,
    organized, generational, identity_locked, global).

% Pastors and priests formed in received-text traditions whose preaching repertoires, memorized verses, and liturgical habits presuppose stable wording. Successive critical decisions withdraw familiar proof-texts and relocate passages; retraining in manuscript evidence is unavailable and unwanted at their career stage. They bear the changes in public, defending or apologizing for texts they did not choose.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, traditionalist_clergy, payer,
    moderate, biographical, identity_locked, regional).

% Ordinary readers who encounter the arrangement only as footnotes, bracketed verses, and quietly altered familiar sentences. They bear the destabilization of devotional life without any seat in the decisions, yet they also receive translations grounded in better evidence than their grandparents had. Individually unorganized, they act on these questions only when a change touches a beloved passage.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_congregations, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, lay_congregations, beneficiary).

% Organizations translating scripture into hundreds of languages. They must build textual-criticism training into translator preparation and absorb the cost of keeping mother-tongue teams current with base-text revisions; in exchange they gain a defensible shared foundation that keeps denominational partners in one project. Their position is genuinely two-sided: heavy compliance costs, real coordination gains.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, mission_translation_agencies, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, mission_translation_agencies, beneficiary).

% Fast-growing churches in Africa, Asia, and Latin America whose received-text attachments and pastoral concerns entered editorial deliberations late or not at all. Translation decisions shaping their worship are made in European and North American institutes; their objections arrive after the fact, as feedback rather than participation.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, majority_world_churches, excluded,
    organized, generational, constrained, continental).

% Historians of religion and secular academics who use the critical texts as evidence about ancient literature and religious movements. They take no side in the confessional dispute, consume the regime's outputs, and can describe its authority structure from outside the commitments that animate the other seats.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, religious_studies_observers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, evidence-defensible textual foundation so that translators, commentators, and clergy across denominational lines work from the same reconstructed text instead of competing ecclesial texts; resolves, once per variation unit, which wording stands behind doctrine, liturgy, and translation.
% TRANSFER_FUNCTION: Moves interpretive authority and labor toward those who can adjudicate manuscript evidence: every structural and meaning decision must pass through credentialed mediation, transferring prestige, funding, edition revenue, and gatekeeping power to the scholarly guild, while transferring the costs of verse-level destabilization onto confessional communities, clergy, and lay readers.
% ABSENT_VOICES: Confessional laity, traditionalist clergy, and majority-world churches who pray from the affected texts had no seat in the editorial rooms; received-text defenders appear in the record chiefly as objects of study rather than participants. Their standing objection - that textual stability is itself a pastoral good worth pricing - was never weighed inside the arrangement that displaced it.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, every modern translation would lose its declared base text, seminary curricula would lose their foundation course, commentaries would lose their citation anchor, and denominations would revert to competing received texts - reopening the pre-critical landscape of mutually anathematizing wordings that the arrangement exists to prevent.
% FOUNDING_PROBLEM: Manuscript witnesses disagree in tens of thousands of places; the printed texts available before systematic criticism descended from a handful of late, hastily assembled manuscripts; no principled procedure existed for deciding which variant reading stood behind any given verse, so translations presented contingent late wordings as apostolic.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: received-text and majority-text critics affirm the original variant problem was real while disputing the solution; Catholic and Orthodox magisterial statements acknowledge the multiplicity of witnesses; practitioners of both sibling translation philosophies accept that variant readings exist and must somehow be adjudicated. No serious party denies the founding problem; the contest is over the remedy, which is itself signal that the problem attestation is not self-serving.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the regime's receipts (edition revenue, endowed chairs, citation economies, gatekeeping over what counts as scholarly) demonstrably accrue to the scholarly guild, while its largest costs (verse-level destabilization of received texts) fall on communities outside the guild - but the coordination product (a shared, evidence-defensible base text) is real and widely consumed, so the base rate sits mid-range rather than high. Suppression 0.48 is authored as a raw structural property, unscaled by power or scope: received-text communities are not coerced by force, but within accredited institutions, mainstream translation projects, and academic publishing, non-critical approaches face hiring, accreditation, and review barriers; the mechanism is predominantly structural (institutional gates), with a secondary internalized component (formation-shaped intuitions that non-critical work is unscholarly). Theater ratio 0.26: apparatus completeness conventions retain readings of negligible weight for diplomatic reasons, and edition launches carry ceremonial weight, but the core philology is functional. Accessibility collapse 0.35: understanding the regime does not eliminate the alternatives - the sibling translation philosophies remain live positions - so alternatives persist rather than collapsing. Resistance 0.6: organized, durable, and doctrinally motivated (received-text movements, majority-text advocacy, liturgical bodies), though fragmented across traditions. The temporal series runs on one shared grid (nine points, all three metrics at every point): extraction and enforcement climbed together as the regime extended from seminars into pew-facing translation and accreditation (peak around 1983), then eased as digital manuscript access lowered exit costs for confessional alternatives, with a partial re-tightening as newer genealogical methods raised the expertise bar. The arc is a ratchet followed by partial erosion, not a cycle - no intermittent-reinforcement mechanism is implicated.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently from identical structural data. From the committee and guild seats, the arrangement is enabling infrastructure they built and legitimately steward: deference to their competence is the price of a trustworthy text. From the identity-locked confessional seats, the same arrangement operates as imposed destabilization: decisions that rewrite their doctrinal proof-texts are made by people who do not share their commitments, and exit (abandoning the received text) equals repudiating the community's self-understanding. Lay congregations sit between: they bear the visible changes without representation yet also receive the more defensible text. Coalition potential among payer classes exists (cross-tradition received-text conferences demonstrate it) but doctrinal differences between preservationist Protestant, Orthodox, and liturgical constituencies fragment it. The engine computes this per-seat divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the guild-side seats toward the beneficiary end: academic_biblical_scholarship (declared beneficiary, identity_locked exit) derives a strongly negative-direction d - the regime subsidizes it and exit would cost it its profession. critical_edition_committees combine agenda-setting with benefit collection. On the target side, confessional_received_text_communities and traditionalist_clergy carry identity_locked exits on top of victim declarations, placing them near the full-target end: trapped-or-locked targets sit nearer full extraction than mobile ones. lay_congregations are declared victims but diffuse and partly compensated, moderating their derived d below the confessional seats. mission_translation_agencies are declared victims yet genuinely dual-positioned (they pay training costs and gain a defensible shared base), so their true d sits mid-range; the derivation from the victim declaration alone would overshoot, and I have left the derivation to run rather than override, accepting the residual imprecision and noting it here. No directionality overrides are authored: the beneficiary/victim plus exit data produce approximately correct directionalities for every seat, and the one known deviation (mission agencies) is documented rather than patched. Spatial scopes are mostly global, which modestly amplifies effective extraction at the payer seats through verification difficulty - fitting, since the communities bearing the costs are geographically dispersed from the decision centers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - manifold variant readings with no principled selection rule, and translations printed from late manuscripts presented as apostolic wording - remains live: new witnesses and new methods keep reopening wording decisions, so the mandate has not outlived its function and mandatrophy is not resolved. The classification guards against mislabeling in both directions: reading the arrangement as pure extraction erases why serious philologists across confessional lines defend it (the shared evidential basis is genuinely produced and consumed); reading it as pure coordination erases the identifiable payers whose received texts are demoted without consent and whose objections were structurally absent from editorial rooms. The tangled-rope shape holds both facts. Receipt surface: gains demonstrably accrue to the academic guild (named seat), and fixing is prohibitive for whoever could fix it - dismantling the regime would strand two centuries of apparatus, curricula, and cross-denominational translation agreements, and would reopen denominational text wars, costs the beneficiaries themselves do not bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the kernel biblical_source_text: the critical-reconstructive reading, which locates primacy in historical recovery of the hypothetical autograph and forbids privileging structure or meaning before the textual basis is established. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative structural analysis across the three readings: formal_equivalence_reading relocates primacy to fidelity of translated structure and re-centers teaching offices as the site of intelligibility; dynamic_equivalence_reading relocates primacy to communicative effect and re-centers mission agencies and lay readership. The disagreement is located in (a) what counts as ''the text'' (recovered autograph vs. transmitted ecclesial form vs. communicative effect) and (b) whose competence adjudicates it.',
    'Adopting a sibling reading re-partitions the beneficiary and payer sets: formal equivalence restores standing to received-text custodians; dynamic equivalence shifts costs toward translators and gains toward target-language communities. This reading''s classification holds only for the critical-reconstructive instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are other constraints.').

omega_variable(
    autograph_recoverability_ceiling,
    'Is the hypothetical original text recoverable to a degree sufficient to ground binding wording decisions, or does the recovery ideal outrun what the witness evidence can support?',
    'Convergence testing of competing reconstructive methods (coherence-based genealogical method, thoroughgoing eclecticism, Byzantine-priority) on shared variation units; published sensitivity analyses of how often method choice changes the printed text.',
    'A low recoverability ceiling converts expert deference into unfalsifiable gatekeeping - payments flow to conjectures no evidence can check - pushing payer-seat classifications toward harder extraction; a high ceiling strengthens the coordination claim and softens the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autograph_recoverability_ceiling, empirical, 'Whether the regime''s authority claim tracks actual evidential reach.').

omega_variable(
    destabilization_cost_distribution,
    'Who concretely bears the pastoral costs of verse-level instability (omitted pericopes, relocated passages, withdrawn proof-texts), and are those costs compensated through retention options, footnoting practice, or liturgical accommodation?',
    'Cross-denominational survey of lectionary and liturgy revision conflicts, translation-adoption disputes, and congregational responses to critical footnotes; measure whether affected communities received any offsetting accommodation.',
    'Concentrated, uncompensated destabilization costs raise effective extraction at the payer seats and harden the asymmetric-extraction half of the classification; diffuse, absorbed costs support a milder coordination-cost reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(destabilization_cost_distribution, empirical, 'Distribution and compensation of the regime''s destabilization costs.').

omega_variable(
    expertise_gate_necessity,
    'Is credentialed expert mediation between the manuscript evidence and the printed text a necessary coordination cost, or a constructed rent that simpler apparatuses and open digital tools could dissolve?',
    'Controlled comparison of decision quality using open-access manuscript transcriptions and simplified apparatuses versus full critical editions with specialist mediation; track error rates and dispute rates.',
    'Demonstrated necessity supports the genuine-coordination component; demonstrated substitutability exposes the gate as rent collection and shifts the classification toward the extraction-dominant pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_gate_necessity, conceptual, 'Necessity versus constructedness of the expertise gate.').

omega_variable(
    cs_framing_underdetermination,
    'Is the operative kernel the methodological regime itself (reconstruction procedures, apparatus conventions), or the legitimacy claim layered above it - that disciplined criticism yields ''the text'' - such that the authority structure, not the method, is what stabilizes commitment?',
    'Test which framing better predicts behavior: if challenges to specific methodological choices (e.g., genealogical method opacity) are absorbed without authority loss, the layered legitimacy claim is the operative kernel; if method challenges directly erode deference, the method itself is the kernel.',
    'Under the layered-legitimacy framing, the authority structure carries more of the extraction weight and the classification tilts toward the enforcement-heavy pole; under the method-as-kernel framing, the coordination function carries more weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of what the stabilized commitment is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 1881, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1881, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement(bibl_tr_t1908, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1908, 0.12).
narrative_ontology:measurement(bibl_tr_t1937, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(bibl_tr_t1955, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1955, 0.18).
narrative_ontology:measurement(bibl_tr_t1966, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1966, 0.21).
narrative_ontology:measurement(bibl_tr_t1983, biblical_source_text__critical_reconstructive_reading, theater_ratio, 1983, 0.24).
narrative_ontology:measurement(bibl_tr_t2000, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(bibl_tr_t2018, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(bibl_tr_t2025, biblical_source_text__critical_reconstructive_reading, theater_ratio, 2025, 0.26).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1881, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1881, 0.44).
narrative_ontology:measurement(bibl_be_t1908, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1908, 0.47).
narrative_ontology:measurement(bibl_be_t1937, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1937, 0.51).
narrative_ontology:measurement(bibl_be_t1955, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1955, 0.54).
narrative_ontology:measurement(bibl_be_t1966, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1966, 0.59).
narrative_ontology:measurement(bibl_be_t1983, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 1983, 0.62).
narrative_ontology:measurement(bibl_be_t2000, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement(bibl_be_t2018, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(bibl_be_t2025, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1881, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1881, 0.3).
narrative_ontology:measurement(bibl_su_t1908, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1908, 0.34).
narrative_ontology:measurement(bibl_su_t1937, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1937, 0.41).
narrative_ontology:measurement(bibl_su_t1955, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1955, 0.49).
narrative_ontology:measurement(bibl_su_t1966, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1966, 0.57).
narrative_ontology:measurement(bibl_su_t1983, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 1983, 0.63).
narrative_ontology:measurement(bibl_su_t2000, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(bibl_su_t2018, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2018, 0.51).
narrative_ontology:measurement(bibl_su_t2025, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, resource_allocation).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the biblical source text' covers three structurally distinct commitments that share one kernel. This story (critical_reconstructive_reading) authors the regime in which textual-basis establishment precedes and constrains every structural or meaning decision; the formal-equivalence and dynamic-equivalence siblings author the translation-philosophy regimes that operate downstream of a settled basis. Epsilon differs across the family because each reading assesses its own arrangement by its own lights: this reading's epsilon prices expert-mediated recovery and confessional destabilization; the formal-equivalence sibling would price structural fidelity against intelligibility losses; the dynamic-equivalence sibling would price communicative yield against source-form losses. The upstream reading influences both siblings by changing their legitimacy conditions - any translation project in its shadow must first credential its base text - without logically ruling either sibling out.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
