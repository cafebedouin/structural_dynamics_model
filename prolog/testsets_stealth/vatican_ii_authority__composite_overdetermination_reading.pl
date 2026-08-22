% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination Reading of Vatican II Interpretive Authority
 *   domain: theological/ecclesiological
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination_reading of the
 *   vatican_ii_authority kernel: the claim that Vatican II is not a single
 *   interpretable event but an overdetermined composite of distinct doctrinal
 *   shifts carrying incompatible theological rationales, such that its
 *   ambiguities cannot be resolved into either continuity or rupture. The
 *   standing arrangement under contest - and therefore the epsilon referent -
 *   is the post-conciliar interpretive regime itself: a magisterial office
 *   obligated to state what the council taught, operating over texts whose
 *   drafting history embeds factional compromise, with a scholarly apparatus
 *   functioning as the de facto adjudicator of what the texts can bear.
 *   Assessed by this reading's own lights, that regime imposes a permanent
 *   irresolution cost on institutional authority and on the faithful who need
 *   settled teaching, while subsidizing a specialist class whose standing
 *   rests on managing the complexity. CONSTRAINT FAMILY NOTE: the colloquial
 *   label 'what Vatican II means' decomposes, per the epsilon-invariance
 *   principle, into three structurally distinct claims - continuity_reading
 *   (organic development, negligible extraction from a settled deposit),
 *   rupture_reading (substantive break, extraction concentrated on the
 *   council's legitimacy itself), and this composite reading (irresolvable
 *   overdetermination, extraction concentrated on univocal interpretive
 *   authority). Each gets its own file, its own epsilon, its own victim set;
 *   they are linked through network.affects_constraints. The claim/metric
 *   relationship here follows the independence rule: claimed_type is authored
 *   from what I take to be structurally true (a genuine coordination function
 *   joined to asymmetric extraction), and the metrics are authored from what
 *   I take to be descriptively true of the regime's operation - including a
 *   receipt surface that reads capture-flavored, which the engine should
 *   weigh against my claim rather than have me reconcile away.
 *
 * KEY AGENTS:
 *   - academic_conciliar_scholars: Primary beneficiary and de facto administrator (organized/constrained) - collects interpretive standing; runs the journals, reviews, hiring, and conference circuits that maintain the reading
 *   - magisterial_doctrinal_authorities: Primary target (institutional/trapped) - bears the cost of every failed attempt at definitive interpretation; cannot decline to interpret its own council
 *   - faithful_seeking_clear_teaching: Diffuse target (powerless/constrained) - bears the practical costs of unsettled liturgical, ecumenical, and catechetical ground
 *   - traditionalist_communities: Excluded disputant (organized/trapped) - objects from outside the formal conversation; its exclusion is partly what the managed-ambiguity settlement rests on
 *   - secular_church_historians: Analytical observer (moderate/analytical) - documents the drafting coalitions and reception battles without a stake in which reading prevails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.56).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.4).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Composite Overdetermination Reading of Vatican II Interpretive Authority").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theological/ecclesiological").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'c7217c61-ecf2-445a-b71e-ca2aa79b9d38').
narrative_ontology:cs_kernel_codification('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', fixed_text).
narrative_ontology:cs_authority_grounding('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', expertise).
narrative_ontology:cs_interpretation_layer_present('c7217c61-ecf2-445a-b71e-ca2aa79b9d38').
narrative_ontology:cs_reading_relation('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', vatican_ii_authority__rupture_reading, influences).
narrative_ontology:cs_axiom('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', foundational, conciliar_ambiguity_irresolvable_by_univocal_synthesis).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_irresolvable_by_univocal_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', conciliar_ambiguity_irresolvable_by_univocal_synthesis, empirically_contingent).
narrative_ontology:cs_axiom('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', secondary, postconciliar_conflicts_structurally_determined).
narrative_ontology:cs_axiom_status(postconciliar_conflicts_structurally_determined, holdable).
narrative_ontology:cs_axiom_grounding('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', postconciliar_conflicts_structurally_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', overdetermined_factional_composite).
narrative_ontology:cs_drift_state('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', contemporary_postconciliar_contestation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c7217c61-ecf2-445a-b71e-ca2aa79b9d38', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_doctrinal_authorities).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_clear_teaching).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, source_critical_redaction_history).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, pluralist_doctrinal_hermeneutics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historians and theologians at universities and pontifical faculties who study the council's drafting history. They publish the editions, monographs, and commentaries that establish what the texts can bear; they staff the journals, review panels, and hiring committees through which interpretive standing is granted. Their authority rests on command of the archival record of factional dispute - the relaciones, emendationes, and roll-call votes. Leaving the specialty means forfeiting decades of accumulated linguistic and archival expertise, and adopting a simple univocal narrative of the council would erase the distinctive contribution their entire training supports.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars, agenda_setter).

% The Roman curial offices and episcopal doctrine committees charged with stating what the council taught. Each attempt at a definitive synthesis draws documented objections grounded in the drafting record, and successive interventions have narrowed rather than closed the disputed questions. The office cannot decline to interpret a council it promulgated as binding, cannot repudiate the texts, and cannot delegate the duty away; its options are repeated expensive closure attempts or a strategic silence that concedes the field to others.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_doctrinal_authorities, payer,
    institutional, generational, trapped, global).

% Pastors, catechists, teachers, and laypeople who need to know what the council changed and why. They receive incompatible summaries from different reputable voices, absorb the practical costs of unsettled liturgical and ecumenical practice in their parishes and schools, and possess no channel through which to compel a resolution. Some conclude the question is unanswerable and disengage; most accommodate, passing the uncertainty to the next generation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_clear_teaching, payer,
    powerless, biographical, constrained, global).

% Communities in irregular or strained canonical standing who read the council as a break with what came before. They stand outside the formal interpretive conversation, yet their objections repeatedly force the question back onto the institutional agenda. Their position is defined by refusal of every settlement on offer: they cannot accept the continuity account without dissolving their reason for existing, and they cannot accept the managed-ambiguity settlement because it concedes the council's bindingness while denying them the verdict they seek.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_communities, excluded,
    organized, generational, trapped, global).

% Academics outside confessional commitments who document the drafting coalitions, the vote margins, and the reception battles. They hold no stake in which reading prevails and no duty to defend any settlement, which makes them the closest available external check on both the guild's and the magisterium's accounts of what the record shows.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, secular_church_historians, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared map of where and why the council's texts resist univocal reading - which passages carry which faction's rationale, which compromises produced which ambiguities - so that historians, translators, and teachers coordinate their labor instead of relitigating from scratch whether disagreement reflects bad reading or textual reality.
% TRANSFER_FUNCTION: Moves interpretive standing from the offices authorized to define teaching to the specialists who document the drafting record: each failed attempt at definitive closure transfers further credibility to source-critical expertise, along with the chairs, conferences, and publications that follow it.
% ABSENT_VOICES: Traditionalist communities outside full communion would object that the ambiguity is itself the pathology and demand resolution rather than management; progressive readers of a rupturist bent would object that the composite framing dissolves their specific error-claims into general untidiness. Both stand outside the scholarly-magisterial conversation - one canonically irregular, the other marginal to the guilds - so the working consensus that the ambiguities are manageable is formed largely among parties with stakes in managing them.
% DISAPPEARANCE_RATIONALE: If the composite framing vanished overnight, the post-conciliar conflicts would be reattributed - to bad faith, incompetence, or conspiracy - rather than recognized as structural; the scholarly field would lose its organizing problem and fragment into warring apologetics; and the magisterium would face immediate renewed pressure to issue the definitive interpretation it has repeatedly failed to stabilize, with the same costly results as before.
% FOUNDING_PROBLEM: By the 1970s neither available account fit the record: organic continuity could not explain why the same documents authorize positions their pre-conciliar counterparts had condemned, and rupture could not explain why the disputed texts passed with near-unanimous votes after explicit factional negotiation. The composite reading was built to explain why intelligent, well-informed readers kept producing incompatible accounts of the same sixteen documents.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the magisterial offices themselves attest the problem is live - their repeated interpretive interventions (the 1985 Extraordinary Synod, the 2005 hermeneutic address, successive curial responses to new controversies) function as running admissions that closure has not been achieved; secular historians unaffiliated with either guild or magisterium document the factional redaction votes in the published acta; and the excluded traditionalist communities attest the ambiguity is real even while denying it is irresolvable.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56: the regime's base cost is substantial but not predatory - the magisterium pays a recurring definition-attempt tax (every synthesis provokes documented objections from the drafting record), and the laity pay a clarity tax, while the coordination delivered is real. Suppression 0.40: alternatives are not eliminated - both sibling readings remain live positions held by real parties - but naive univocal readings are gated by peer review, hiring, and imprimatur politics; suppression here is mostly structural (career and canonical consequences) rather than internalized. Theater 0.27: the archival and philological work is genuine, but a growing share of activity is commemorative and citational ritual. Accessibility_collapse 0.50: once the composite structure is understood, the 'the text simply means what it says' alternative collapses, but the two sibling readings survive intact - alternatives half-collapse. Resistance 0.62: sustained counter-programs exist (the hermeneutic-of-continuity initiative, traditionalist rejection, periodic synodal closure attempts), and each has failed expensively. Temporal shape (interval 0-60 approximates 1965-2025; all three series share the one grid {0,10,20,30,40,50,60}): extractiveness rises through the consolidation decades, peaks around the era of the most expensive closure attempts, then partially retreats as the magisterium adapts by strategic ambiguity - declining to define is cheaper than failing to define. Suppression_requirement is traced because enforcement capacity genuinely moved: gatekeeping consolidated through the middle decades, then eroded as the guild contracted and digital discourse escaped journal control. Coalition note: the powerless laity seat is not hopeless - lay movements have periodically coalesced to demand definitive liturgical and moral clarification, and that latent coalition is the main reason suppression cannot ratchet much higher. Suppression is authored as a raw structural property; only extractiveness is context-scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the magisterial seat the arrangement is an enforced irresolution: an office with a definitional duty bound to texts that defeat definition, trapped by its own claim to interpretive competence. From the scholarly seat the same arrangement is a productive research program: the irresolvability is not a defect but the discovery, and the guild's administration of it is service, not gatekeeping. From the lay seat it is an abandoned question: neither a mystery to be revered nor a problem being worked, but a cost silently distributed to those least equipped to refuse it. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims are declared in base_properties and mirrored on the stakeholder surface. academic_conciliar_scholars sits near the beneficiary end (d low): the regime subsidizes their standing, and although they bear maintenance costs, they are net collectors - the derivation needs no override. magisterial_doctrinal_authorities sits near the full-target end (d high, amplified by trapped exit): the office cannot exit its own interpretive duty, so the extraction lands at nearly full weight. faithful_seeking_clear_teaching carries high d with constrained exit - high per-person burden, no arbitrage, but diffuse organization. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct relationships, and the one subtlety (the scholars' dual beneficiary/administrator position) is captured by the secondary_role declaration rather than a d correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Against pure-extraction readings: the coordination function is live, not vestigial - the composite map genuinely prevents infinite relitigation and correctly predicts where interpretive fights recur, so this is not a snare wearing a scholarship costume; the falsifiability probe in omega guild_rent_preservation_drift is the standing test. Against pure-coordination readings: the extraction is asymmetric and enforced - the same structure that coordinates the guild systematically strips closure authority from the magisterium, which is why requires_active_enforcement is declared. It is not a piton: the founding problem is live (founding_problem_status: live), theater is low, and the administrator's cost-asymmetry test fails because the guild could not cheaply change the arrangement even if it wished - the texts bind it too. The R5 mismatch consumer should find no zombie flag: live founding problem crossed with world_rearranges is the consistent cell, so mandatrophy_resolved is deliberately not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_family_membership,
    'This constraint instantiates one reading (composite_overdetermination_reading) of the vatican_ii_authority kernel; do the sibling readings (continuity_reading, rupture_reading) instantiate structurally distinct constraints with distinct epsilon values and distinct beneficiary/victim sets, or does the family collapse into one structure described three ways?',
    'Author and compare the sibling stories: check whether their beneficiary/victim structures, epsilon referents, and enforcement profiles diverge materially from this story''s.',
    'Material divergence confirms the family decomposition and this story''s isolation of a single stable epsilon; convergence would indicate the decomposition is verbal and the three readings should be merged into one constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_family_membership, conceptual, 'Committer structure: one reading of a three-reading kernel; family-level validity of the decomposition.').

omega_variable(
    irresolvability_vs_underdetermination,
    'Are the council''s textual ambiguities genuinely irresolvable (incompatible rationales baked into the promulgated texts themselves) or merely underdetermined (open to an authoritative completion that no one has yet supplied)?',
    'Exhaustive redaction-history and reception analysis: if every candidate univocal synthesis contradicts some promulgated text or an approved draft rationale, the ambiguity is irresolvable; if a textually open synthesis remains available, it is underdetermined.',
    'If underdetermined, the burden shifts from textual structure to magisterial will, weakening this reading''s charge against institutional authority and moving the arrangement toward plain coordination around an unfinished task.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irresolvability_vs_underdetermination, empirical, 'The load-bearing distinction separating this reading from both siblings.').

omega_variable(
    guild_rent_preservation_drift,
    'Does the scholarly guild''s accrual of standing from maintained ambiguity indicate that the explanatory function is decaying into rent preservation?',
    'Falsifiability audit: track whether the guild updates or abandons the composite thesis in response to disconfirming archival findings, and whether concrete resolution proposals receive substantive engagement rather than sociological dismissal.',
    'Confirmed rent preservation would push the arrangement toward pure extraction with the coordination story as cover; sustained responsiveness to evidence would confirm the coordination function is live and the hybrid classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guild_rent_preservation_drift, empirical, 'Capture-drift probe on the beneficiary seat that also administers the reading.').

omega_variable(
    kernel_location_texts_vs_reception,
    'Is the kernel located in the promulgated texts (the fixed_text framing adopted here) or in the living reception practice of the church (a rival framing under which practice could progressively resolve the ambiguity)?',
    'Classify the arrangement under both framings and compare: if the reception-practice framing yields a resolvable, practice-coordinated arrangement, the fixed-text framing is doing decisive classificatory work. Signals guiding the fixed-text choice include the documents'' formal promulgation status and the magisterium''s own practice of arguing from drafted text rather than accumulated usage.',
    'Under the reception framing, this reading''s irresolvability premise weakens and its classification would shift toward transitional coordination; under the fixed-text framing it stands as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_location_texts_vs_reception, conceptual, 'CS-framing under-determination: text-kernel versus reception-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.27).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, information_standard).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the meaning of Vatican II' covers three structurally distinct claims with different epsilon values, different beneficiary/victim structures, and different failure modes. continuity_reading (upstream, highest empirical confidence in its own domain) is routinely cited as evidence against this reading; rupture_reading shares this reading's recognition of contradiction but locates it between council and tradition rather than within the drafting coalitions. This story links both siblings; each sibling file should link back. The upstream/downstream pattern runs continuity -> composite -> rupture in citation practice, mirroring the BGS family pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
