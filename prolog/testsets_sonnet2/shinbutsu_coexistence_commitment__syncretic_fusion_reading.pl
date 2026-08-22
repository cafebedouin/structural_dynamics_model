% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Ontological Fusion (Syncretic Fusion Reading)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the syncretic-fusion reading of the
 *   shinbutsu-coexistence kernel: honji suijaku is treated as a genuine,
 *   doctrinally coherent claim that kami are local manifestations (suijaku)
 *   of universal Buddhist truths (honji). Under this reading, the fusion is a
 *   single ontology, not a working truce between separate domains and not an
 *   ad hoc bundle. The theological elite of Shingon and Tendai lineages hold
 *   interpretive authority over the correspondence charts, and the jinguji
 *   (combined shrine-temple) institution is the structural embodiment of the
 *   fused cosmology — one clergy legitimately governing what is, under this
 *   reading, one underlying reality wearing two faces. ε is authored for the
 *   standing honji-suijaku arrangement as this reading's own theological
 *   tradition understands and defends it: a real coordination function
 *   (resolving a genuine cosmological tension between an imported
 *   universalist religion and entrenched local cults) riding alongside
 *   genuine asymmetric extraction (subordination of independent kami
 *   priesthoods, absorption of shrine revenue, ritual-fee dependency imposed
 *   on lay worshippers). This is NOT the endorsed alternative of either
 *   sibling reading — it is the fusion arrangement as its own defenders would
 *   describe it, including its costs.
 *
 * KEY AGENTS:
 *   - shingon_tendai_theological_elite: interpretive authority setting and revising the honji-suijaku correspondence system
 *   - jinguji_administering_monks: institutional beneficiaries whose office is structurally created by the fusion claim
 *   - buddhist_temple_networks: institutional beneficiaries absorbing shrine land and lay allegiance
 *   - shrine_priests_subordinated_to_temples: bear the cost of demotion to administering a 'provisional manifestation'
 *   - local_kami_cult_communities: bear the cost of cosmological reinterpretation without consultation
 *   - lay_worshippers_denied_direct_kami_access: bear increased ritual dependency and fees
 *   - meiji_state_religious_reformers: excluded voice whose later rupture retroactively contests the fusion's coherence
 *   - comparative_religion_scholars: analytical observer assessing genuineness vs. institutional convenience of the synthesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.52).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Ontological Fusion (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'de2f02bb-7261-4de4-84d6-1048b3ebf984').
narrative_ontology:cs_kernel_codification('de2f02bb-7261-4de4-84d6-1048b3ebf984', formalized).
narrative_ontology:cs_authority_grounding('de2f02bb-7261-4de4-84d6-1048b3ebf984', lineage).
narrative_ontology:cs_interpretation_layer_present('de2f02bb-7261-4de4-84d6-1048b3ebf984').
narrative_ontology:cs_reading_relation('de2f02bb-7261-4de4-84d6-1048b3ebf984', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('de2f02bb-7261-4de4-84d6-1048b3ebf984', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('de2f02bb-7261-4de4-84d6-1048b3ebf984', foundational, kami_are_local_traces_of_universal_buddha_nature).
narrative_ontology:cs_axiom_status(kami_are_local_traces_of_universal_buddha_nature, holdable).
narrative_ontology:cs_axiom_grounding('de2f02bb-7261-4de4-84d6-1048b3ebf984', kami_are_local_traces_of_universal_buddha_nature, theological).
narrative_ontology:cs_axiom('de2f02bb-7261-4de4-84d6-1048b3ebf984', secondary, single_cosmology_admits_no_independent_kami_domain).
narrative_ontology:cs_axiom_status(single_cosmology_admits_no_independent_kami_domain, holdable).
narrative_ontology:cs_axiom_grounding('de2f02bb-7261-4de4-84d6-1048b3ebf984', single_cosmology_admits_no_independent_kami_domain, theological).
narrative_ontology:cs_reference_frame('de2f02bb-7261-4de4-84d6-1048b3ebf984', heian_period_correspondence_orthodoxy).
narrative_ontology:cs_drift_state('de2f02bb-7261-4de4-84d6-1048b3ebf984', meiji_shinbutsu_bunri_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('de2f02bb-7261-4de4-84d6-1048b3ebf984', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_temple_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_administering_monks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shingon_tendai_theological_elite).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_subordinated_to_temples).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_worshippers_denied_direct_kami_access).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, universal_buddhist_truth_as_ground_of_all_local_deities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, single_coherent_cosmology_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produces and polices the honji-suijaku correspondence charts (which kami is the suijaku of which buddha/bodhisattva), trains the interpretive lineage that adjudicates disputes, and controls the doctrinal vocabulary in which any claim about kami must be phrased to be taken seriously in elite religious discourse. Can revise correspondences when politically convenient and suffers no cost when doing so.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shingon_tendai_theological_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Run the combined shrine-temple complexes (jinguji), collecting revenue, land grants, and ritual authority from administering both the kami rites and the Buddhist services performed on the same grounds. Their institutional position exists only because the ontological fusion licenses one clergy to oversee both cults; a domain-partition reading would eliminate their office.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_administering_monks, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_administering_monks, agenda_setter).

% Absorb shrine lands, worshippers, and tax exemptions into temple estates by virtue of the doctrine that the kami enshrined there is merely a local trace of the temple's Buddha. Gain political legitimacy by presenting themselves as fulfilling, not displacing, indigenous cult practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_temple_networks, beneficiary,
    institutional, generational, arbitrage, national).

% Formerly independent kami ritual specialists now occupy a subordinate rung beneath jinguji monks in many shrine-temple complexes; their authority over the kami they serve is reinterpreted as authority over a mere provisional manifestation, with the monks holding the higher, 'true' referent. Leaving means abandoning hereditary shrine office and community standing built over generations.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests_subordinated_to_temples, payer,
    moderate, biographical, constrained, regional).

% Villages and clans whose ancestral kami worship is reframed, without their doctrinal input, as an incomplete or preparatory stage toward Buddhist truth. Their independent cosmology is not abolished outright but is subordinated within a hierarchy they did not author and cannot contest in the theological register that now governs legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cult_communities, payer,
    powerless, generational, trapped, local).

% Ordinary petitioners at combined shrine-temple sites are told that efficacious access to the kami now properly runs through Buddhist ritual mediation (sutra recitation, mantra, monastic intercession) rather than direct kami rite, increasing the ritual fees and clerical dependency required to secure the same blessings previously sought directly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_worshippers_denied_direct_kami_access, payer,
    powerless, immediate, constrained, local).

% Not present within the honji-suijaku era's own discourse, but their later forcible separation edict (shinbutsu bunri) treats the fused ontology as an artificial accretion to be stripped away, retroactively voicing an objection the fusion's own institutions never had to answer while it held power.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_state_religious_reformers, excluded,
    institutional, generational, analytical, national).

% Study the correspondence charts, temple records, and Meiji-era rupture to assess whether the fusion was a genuine, stable theological synthesis or an institutionally convenient overlay maintained by those who profited from administering both cults simultaneously.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, coherent cosmological framework that lets one clergy administer both kami ritual and Buddhist doctrine at the same site without contradiction, resolving what would otherwise be competing claims to religious authority over the same communities and land.
% TRANSFER_FUNCTION: Moves interpretive authority, land revenue, ritual fees, and lay religious allegiance from independent kami priests and local cult communities toward jinguji-administering monks and the Buddhist temple networks that supply the doctrinal apparatus legitimating the merger.
% ABSENT_VOICES: Local kami cult communities and hereditary shrine priest lineages who held the pre-fusion cosmology are not recorded as parties to the honji-suijaku correspondence charts; the elite theological register in which the fusion is debated (Sanskrit-derived Buddhist categories, esoteric commentary) was largely inaccessible to them, so their assent to being reinterpreted as a 'trace' of Buddhist truth is nowhere solicited.
% DISAPPEARANCE_RATIONALE: If the ontological fusion claim were to vanish, jinguji institutions would lose their doctrinal warrant for administering both shrine and temple, shrine priests would regain unmediated ritual authority over their kami, land and revenue currently pooled under combined administration would need re-partition, and lay worship would no longer require Buddhist ritual mediation for kami-directed petitions — precisely what happened, abruptly and violently, under the Meiji shinbutsu bunri edicts.
% FOUNDING_PROBLEM: Buddhism's arrival in Japan needed to explain its relationship to already-entrenched, locally powerful kami cults without either denying kami reality (which would alienate converts and patrons) or admitting a rival, co-equal cosmology (which would undercut Buddhist universalist claims to be the complete and final truth).
% FOUNDING_PROBLEM_CORROBORATION: Jinguji institutional records and the theological elite's own commentarial tradition attest the fusion as a settled, still-live cosmological truth. Meiji state reformers and modern comparative religion scholarship attest, from outside the fusion's beneficiary institutions, that the 'problem' addressed was primarily one of ecclesiastical and revenue consolidation rather than a genuinely converging cosmology — the abruptness and thoroughness of the 1868 separation, achieved largely by state fiat rather than theological refutation, is cited as evidence the fusion's coherence was institutionally maintained rather than independently compelling.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.52) are moderate-to-substantial rather than extreme: the fusion is not naked expropriation — it does perform real cosmological coordination work (resolving Buddhism's relationship to entrenched kami cults was a genuine problem, not a manufactured one) — but the correspondence charts and jinguji institution also visibly redirect land, revenue, and ritual authority toward the parties who administer the fused doctrine. Theater ratio rises over the interval (0.15 to 0.38) as the correspondence system calcifies into increasingly elaborate, increasingly defended doctrinal machinery whose primary observable activity becomes maintaining institutional prerogative rather than resolving live cosmological tension. Accessibility collapse (0.62) is substantial because, once the fused vocabulary becomes the only legitimate register for discussing kami-Buddha relations among religious elites, alternative framings (domain partition, or plain incoherence) become difficult to articulate within elite institutions — though not impossible, hence not mountain-level collapse. Resistance (0.47) reflects real but contained friction: some shrine lineages and communities push back against subordination, but lack the theological vocabulary or institutional standing to mount an effective counter-claim until the Meiji state's external political intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   The theological elite and jinguji monks sit near the beneficiary end: they author and administer the correspondence system, and it grants them expanded interpretive and institutional territory. Buddhist temple networks likewise benefit through land and lay-allegiance absorption. Shrine priests, local kami communities, and lay worshippers sit near the target end: their prior independent standing (unmediated priesthood, autonomous cosmology, direct ritual access) is what the fusion structurally reassigns to Buddhist mediation. Their exit options are constrained-to-trapped — hereditary office, generational community identity, and lack of an alternative legitimating vocabulary make disengagement costly or unavailable within the era's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how should an imported universalist religion relate to entrenched local cults without either erasing them or conceding a rival cosmology — was genuinely live at the point of origin. Whether it remains live centuries later, once the correspondence system had calcified into settled institutional practice defended chiefly by those it enriched, is exactly the contested status recorded in founding_problem_status. The Meiji rupture — achieved by state decree rather than theological refutation — is the strongest available evidence that the arrangement had drifted from active cosmological coordination toward institutional inertia defended by its beneficiaries; this classification treats that drift as real without asserting it retroactively invalidates the earlier, more genuinely coordinative phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_institutional_convenience,
    'Was the honji-suijaku ontological unification a genuine, internally motivated theological synthesis, or an institutionally convenient doctrine adopted and sustained primarily because it licensed land and revenue consolidation under joint shrine-temple administration?',
    'Comparative analysis of correspondence-chart revisions against documented land-grant and revenue-consolidation events; if doctrinal correspondences shift in step with administrative reorganizations rather than independent theological argument, the institutional-convenience reading gains support.',
    'If genuine synthesis, the coordination function is stronger than authored here and the classification should sit closer to rope; if primarily institutional convenience, the tangled_rope classification understates extraction and the constraint approaches snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_institutional_convenience, conceptual, 'Whether the fusion''s coordination function is theologically autonomous or institutionally instrumental.').

omega_variable(
    committer_kernel_reading_location,
    'This constraint is one reading (syncretic_fusion_reading) of the shinbutsu_coexistence_commitment kernel. Where exactly does the disagreement with the sibling readings (domain_partition_reading, incoherent_bundle_reading) live — is it a factual dispute about historical doctrine, or an interpretive dispute about how to characterize deliberate ambiguity that all readings agree existed?',
    'Close textual analysis of primary honji-suijaku correspondence documents (e.g., Ryobu Shinto and Sanno Shinto texts) for internal consistency claims versus toleration of unresolved multiplicity; cross-reference against how contemporaneous practitioners themselves described the relationship.',
    'If primary sources show practitioners explicitly asserting single-ontology claims, this reading is well-supported as historically dominant; if sources show practitioners tolerating live ambiguity between domain-partition and fusion framings interchangeably, the incoherent_bundle_reading gains ground and this reading''s ''single coherent ontology'' premise weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_location, conceptual, 'Locates the structural disagreement among the three kernel readings in either doctrinal-textual fact or interpretive framing.').

omega_variable(
    meiji_rupture_as_evidence_type,
    'Does the speed and thoroughness of the Meiji-era forced separation (shinbutsu bunri) constitute evidence that the fusion was never doctrinally deep — or merely evidence that a politically motivated state could dismantle even a genuine synthesis through coercive decree regardless of its theological merits?',
    'Examine resistance patterns at the local level during the separation edicts: sustained local defense of fused practice against state pressure would support genuine depth; rapid, largely uncontested local compliance would support the shallow/institutional-convenience reading.',
    'Affects how strongly the founding_problem_status=''contested'' outcome should weight toward ''dead'' (institutional inertia argument) versus ''live-but-suppressed'' (coercive external termination of a still-functioning synthesis).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meiji_rupture_as_evidence_type, empirical, 'Whether Meiji-era rupture speed is evidence against doctrinal depth or merely evidence of state coercive capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 150, 0.2).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 300, 0.26).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 500, 0.31).
narrative_ontology:measurement(shin_tr_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 700, 0.35).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 900, 0.37).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.38).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(shin_be_t150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 150, 0.4).
narrative_ontology:measurement(shin_be_t300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 300, 0.47).
narrative_ontology:measurement(shin_be_t500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 500, 0.53).
narrative_ontology:measurement(shin_be_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 700, 0.56).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 900, 0.58).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(shin_su_t150, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(shin_su_t300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 300, 0.4).
narrative_ontology:measurement(shin_su_t500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 500, 0.45).
narrative_ontology:measurement(shin_su_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 700, 0.49).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 900, 0.51).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_coexistence_commitment kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: syncretic_fusion_reading (this story, tangled_rope — single coherent ontology with theological-elite authority and jinguji institutional embodiment), domain_partition_reading (kami and Buddhas govern separate existential domains without ontological merger — likely lower extraction, more genuinely rope-like coordination), and incoherent_bundle_reading (the arrangement was never coherent but a deliberately ambiguous bundle maintained by institutional power, collapsing under Meiji pressure — likely closer to snare or piton given the collapse evidence). All three link to each other via affects_constraints because they share the same historical institutional substrate (jinguji, honji-suijaku correspondence practice, Meiji rupture) even though each reading authors a structurally distinct ontology and a distinct ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
