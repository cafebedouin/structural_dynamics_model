% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 as Eternal Immutable Law: Plural Sealing Required for Exaltation (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the eternal_marriage_covenant
 *   kernel: the immutable commandment reading, in which D&C 132 (recorded
 *   1843) establishes plural marriage as eternal, immutable divine law
 *   required for exaltation, admitting no legitimate revision. Under the
 *   federal anti-polygamy crusade and the 1890/1904 Manifestos, this reading
 *   produces the expected structural delta: a martyrdom bind in which
 *   practicing the covenant means prosecution and confiscation while
 *   complying with the state means apostasy, and no internal mechanism exists
 *   to soften either horn — any purported revision is, by the reading's own
 *   terms, definitionally illegitimate. The interval maps t=0 to 1843
 *   (recording of the revelation) and t=70 to roughly 1913, a generation
 *   after the Second Manifesto, when communities holding this reading had
 *   consolidated in enclaves. The colloquial label 'Mormon polygamy doctrine'
 *   decomposes into three structurally distinct constraints (this reading
 *   plus the prophetic_override and temporal_accommodation siblings); they
 *   differ on whether the 1843 text carries its own revision valve, which
 *   changes the victim set, the rigidity of the bind, and epsilon. Only this
 *   reading is classified here. KEY AGENTS (by structural relationship): -
 *   sealing_authority_holders: Agenda-setting seat
 *   (institutional/identity_locked) — administers sealings, collects
 *   deference and allocation power - plural_household_heads: Primary
 *   beneficiary (moderate/identity_locked) — receives household labor,
 *   status, promised exaltation; bears legal risk - plural_wives: Primary
 *   target (powerless/identity_locked) — bears the transfer of exclusivity,
 *   labor, and childbearing - adolescent_brides: Most exposed target
 *   (powerless/trapped) — married without standing to refuse -
 *   dissenting_believers: Internal targets (moderate/constrained) — tribunals
 *   and shunning for hesitation - federal_legal_authorities: Excluded
 *   external coercive force (institutional/mobile) — zero standing inside the
 *   framework - religious_history_scholars: Analytical observer — sees the
 *   full structure from archival distance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.86).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 as Eternal Immutable Law: Plural Sealing Required for Exaltation (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '466ca2a0-b7ea-414d-bb3a-33812bc15891').
narrative_ontology:cs_kernel_codification('466ca2a0-b7ea-414d-bb3a-33812bc15891', fixed_text).
narrative_ontology:cs_authority_grounding('466ca2a0-b7ea-414d-bb3a-33812bc15891', lineage).
narrative_ontology:cs_interpretation_layer_present('466ca2a0-b7ea-414d-bb3a-33812bc15891').
narrative_ontology:cs_reading_relation('466ca2a0-b7ea-414d-bb3a-33812bc15891', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('466ca2a0-b7ea-414d-bb3a-33812bc15891', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('466ca2a0-b7ea-414d-bb3a-33812bc15891', foundational, plural_sealing_required_for_exaltation).
narrative_ontology:cs_axiom_status(plural_sealing_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('466ca2a0-b7ea-414d-bb3a-33812bc15891', plural_sealing_required_for_exaltation, theological).
narrative_ontology:cs_axiom('466ca2a0-b7ea-414d-bb3a-33812bc15891', foundational, covenant_law_immutable_against_civil_pressure).
narrative_ontology:cs_axiom_status(covenant_law_immutable_against_civil_pressure, holdable).
narrative_ontology:cs_axiom_grounding('466ca2a0-b7ea-414d-bb3a-33812bc15891', covenant_law_immutable_against_civil_pressure, theological).
narrative_ontology:cs_reference_frame('466ca2a0-b7ea-414d-bb3a-33812bc15891', immutable_1843_revelation_supremacy).
narrative_ontology:cs_drift_state('466ca2a0-b7ea-414d-bb3a-33812bc15891', post_manifesto_institutional_repudiation, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('466ca2a0-b7ea-414d-bb3a-33812bc15891', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, sealing_authority_holders).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, plural_household_heads).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, adolescent_brides).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preside over the councils that authorize and perform plural sealings, judge member worthiness, and decide which marriages proceed. Deference, obedience, and the decisive voice over marriage allocation flow to them. Their office has a reason to exist only if the 1843 text is final: if a later prophet or civil pressure could revise the covenant, the authority they exercise evaporates. Leaving the framework would mean surrendering the only standing they have ever held.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, sealing_authority_holders, agenda_setter,
    institutional, generational, identity_locked, regional).

% Men who take additional wives under the covenant because they understand it as a requirement for the highest degree of heaven. Household labor, companionship, progeny, and community status flow to them. They also carry the legal exposure: federal statutes criminalize their marriages, and conviction has meant prison and confiscation of property. Walking away would forfeit the salvation they believe they have already paid heavily for and sever every relationship they have.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_household_heads, beneficiary,
    moderate, biographical, identity_locked, regional).

% Women sealed into households where the husband has other wives. They share a husband's time and resources, bear repeated childbearing, frequently run households far from natal kin, and are taught from childhood that their eternal standing depends on faithfulness to the sealing. Refusal or departure is framed as forfeiting exaltation and abandoning children and community. Many hold no independent money, and the settlements are remote.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, identity_locked, regional).

% Girls, some well below majority age in the documented historical record, married to older men with parental and council approval. They have no independent resources, no standing to decline a council-approved match, and little contact with anyone outside the settlement. Their realistic options are the marriage or the ostracism of their entire family.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, adolescent_brides, payer,
    powerless, immediate, trapped, local).

% Members who accept the community and much of its teaching but balk at a specific assignment, a specific marriage, or the claim that the covenant binds regardless of civil consequence. They face worthiness tribunals, shunning, and the instruction that hesitation is the first step toward perdition. Physical exit exists — towns, rail lines, wage work outside — but taking it means losing family, community, and, by the framework's own account, their salvation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_believers, payer,
    moderate, biographical, constrained, regional).

% Prosecutors, marshals, and federal courts enforcing anti-bigamy and anti-cohabitation statutes through the 1880s crusade and after. They stand wholly outside the covenant's legitimacy conversation: within a framework that holds the 1843 text final, the state's demand has no standing to be weighed — it can only be suffered or defied. Their objection would require the framework to treat constitutional law as a competing normative authority, which the framework's own terms forbid.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_legal_authorities, excluded,
    institutional, generational, mobile, national).

% Academic historians of American religion working from archival records, court transcripts, diaries, and demographic data. They owe allegiance to no seat, publish outside every community's control, and document both the cohesion the covenant produced for a persecuted people and the costs borne by the women and children inside it.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, religious_history_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, sealing_authority_holders).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds a scattered, persecuted population into one interdependent kinship web: council-allocated plural marriage knits elite families to each other, creates dense mutual obligation, and marks the covenant people off from the surrounding society. It solved community persistence under existential external hostility.
% TRANSFER_FUNCTION: Moves women's marital exclusivity, reproductive capacity, domestic labor, and obedience from women and their natal families to plural households; moves deference, marriage-allocation power, and material contribution upward to the sealing hierarchy.
% ABSENT_VOICES: The women and girls whose marriages were decided in councils they could not address; dissenters removed by tribunals before their objections could accumulate; the federal legal order, which the framework assigns zero adjudicative standing; and, after 1890, the Salt Lake leadership whose repudiation this reading refuses to hear as authoritative.
% DISAPPEARANCE_RATIONALE: If the immutable-law claim vanished overnight, the authority that allocates marriages would dissolve, the marriage market would revert to ordinary choice, the martyrdom economy would collapse (there would be nothing left to suffer for), and the communities would have to re-found identity on something other than covenant distinction. The twentieth-century schisms show this rearrangement beginning wherever the reading lost its hold.
% FOUNDING_PROBLEM: A refugee church driven from Missouri and Illinois, its founder killed, needed a mechanism that would bind scattered converts into one self-perpetuating people and mark them off from the society that had destroyed it. D&C 132 supplied both a soteriology (exaltation runs through plural sealing) and a social technology (every leading family bound to every other by marriage obligation).
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, the cohesion-under-persecution genealogy is corroborated by academic historiography of the period, by the federal court record (Reynolds v. United States and the crusade files), and by demographic studies of nineteenth-century Utah kinship networks. No source outside the benefiting parties attests the soteriological-necessity half of the founding problem; that claim is attested only by the offices and households the arrangement sustains.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86 at interval end) because the transfer borne by the paying seats is decoupled from any reciprocal good delivered to them: the covenant promises benefits payable only after death, deniable only at the price of salvation, while the costs — exclusivity, labor, childbearing, legal jeopardy — are paid now. The series climbs monotonically as the bind tightens: post-Manifesto secrecy raised the cost of every sealing (younger marriages, remote settlements, severed records) without adding anything to the paying seats. Suppression (0.90) is dual-sourced — internal machinery (damnation teaching, worthiness tribunals, shunning) plus external compression (federal statutes closing the practice's legal space) — and per the framework it is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Theater ratio (0.45) tracks the post-Manifesto gap between public denial and private administration: before 1890 the arrangement was performed openly and theater was low; afterward a growing share of activity consisted of maintaining the public fiction while the councils continued, which is concealment with a performative face. Accessibility collapse (0.85) is high because within the reading's own frame, understanding the constraint destroys alternatives: rejection of plural sealing forfeits exaltation by definition, so the only exits are total (leave everything, lose everyone, burn). Resistance (0.65) reflects the federal crusade, sustained internal dissent, and the eventual institutional repudiation by the Salt Lake church. The claimed type is tangled_rope: the covenant performed a real coordination function — it is a large part of why this community survived pressures that destroyed comparable movements — while the same structure asymmetrically extracted from women and children under active enforcement. The claim and the metrics are authored independently; if the engine computes the payer seats as snare-flavored given suppression at 0.90 and a closed revision path, that divergence is the measurement, not an error. Coordination type is declared identity_coordination with its eyes open: the identity narrative here is load-bearing (boundary maintenance of a covenant people) AND extractive, which is exactly the FNL gaming risk the conservative 0.08 floor exists to catch — the coupling test should scrutinize whether identity framing excuses extraction concentrated on the powerless at regional scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical doctrine. From the sealing_authority_holders seat, the arrangement is sacred stewardship: they experience cost as sacrifice and the no-revision-path as fidelity, not rigidity. From the plural_household_heads seat, it is a costly blessing — real legal risk absorbed willingly against an infinite promised return. From the plural_wives and adolescent_brides seats, the same structure operates as confinement with a theological lock: the identical teaching that anchors the men's courage anchors the women's inability to leave. Same-level differentiation is sharpest between husbands and wives inside one household: nominally equal members of one covenant, they hold opposite directionalities, different exit structures (his identity lock is chosen and ideologically loaded; hers is imposed and economically enforced), and different time horizons (he banks exaltation; she pays biographically). The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: sealing_authority_holders and plural_household_heads sit near the beneficiary end (low d), with the authority holders nearest zero since the arrangement subsidizes their entire standing. The victim declarations place plural_wives, adolescent_brides, and dissenting_believers near the target end (high d), amplified by exit structure: adolescent_brides (trapped, local scope) sit nearest full-target; plural_wives (identity_locked) close behind, since identity lock places an agent near the full-target end even where physical exit technically exists; dissenting_believers (constrained) slightly less. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the two seats the derivation cannot reach — federal_legal_authorities (excluded, outside the framework's normative universe) and religious_history_scholars (analytical) — are correctly handled as non-deriving seats rather than corrected by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. A pure-extraction reading would miss why generations defended this arrangement under lethal pressure: the coordination output was real, measurable in community survival, and sincerely experienced as covenant — flattening it to snare erases the believers' agency and the genuine collective-action problem solved. A pure-coordination reading would erase the wives and girls whose exclusivity, labor, and childhood were the transferred substance. The R5 interview locates the atrophy precisely: the persecution-era mandate (bind a hunted people) died with the crusade's end, while the soteriological mandate (exaltation requires plural sealing) is unfalsifiable from inside the framework and therefore cannot die on its own — hence founding_problem_status 'contested' rather than a clean mandatrophy declaration. The arrangement persists on a mandate that is half-dead and half-unfalsifiable, and the no-revision-path structure guarantees it cannot retire the dead half. That structural inability to sunset is the reading's signature and the main thing separating it from its two siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_eternal_marriage_kernel,
    'This constraint is the immutable_commandment_reading of the eternal_marriage_covenant kernel. What changes structurally if the same kernel is instantiated as the prophetic_override_reading or the temporal_accommodation_reading, and where exactly is the disagreement located?',
    'Comparative doctrinal analysis across the three readings: locate whether the 1843 text is held to carry its own revision authority (override), to be suspendable in practice while retained in principle (accommodation), or to be absolutely final (this reading). The disagreement lives entirely in the revision-valve question, not in the text itself.',
    'Sibling readings change the victim set and the bind: the accommodation reading narrows ongoing harm to the suspended-practice period; the override reading recentralizes harm in the living prophet''s discretion and dissolves the martyrdom bind entirely. This reading uniquely forecloses both valves, which is what produces the martyrdom constraint and the schism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_eternal_marriage_kernel, conceptual, 'Committer structure: kernel membership, reading instantiation, sibling deltas, and the located point of disagreement (existence of a legitimate revision path).').

omega_variable(
    revision_path_existence,
    'Is there ANY legitimate revision path inside this reading — for instance, a new revelation the enclave communities could accept — or is immutability absolute such that no internal mechanism can ever soften the bind?',
    'Examine the reading''s own treatment of claimed post-1890 revelations and of the 1886 account of Joseph Smith: if the community''s criteria make acceptance of any superseding revelation definitionally impossible, the path is closed; if some criterion exists, the constraint retains a residual valve.',
    'If a path exists, the constraint softens toward the accommodation reading''s structure and the suppression series should eventually turn down; if none exists, the martyrdom bind is permanent and the payer seats'' computed classification trends toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_path_existence, conceptual, 'Whether the reading''s immutability claim is absolute or admits a residual internal revision mechanism.').

omega_variable(
    consent_under_salvation_conditionality,
    'Does consent to a plural sealing count as meaningful consent when the framework teaches that refusal forfeits exaltation?',
    'Compare entry decisions across cohorts differing in exposure to the damnation teaching and in economic independence; use oral histories of women who left, attending to whether they describe their younger selves as having had a live alternative.',
    'If consent is structurally compromised by the salvation-stakes framing, effective extraction exceeds the authored value and the victim set widens from the documented cases to the full covenant female population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_salvation_conditionality, empirical, 'Whether consent obtained under conditional-salvation framing is structurally valid.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression on plural wives structural (geographic isolation, economic dependency, legal jeopardy) or internalized (damnation terror, covenant identity fused with self-concept), and in what proportion?',
    'Post-exit suppression trajectory among women who leave: if fear of damnation, identity collapse, and grief persist after the structural barriers fall, the internalized share is substantial; if leavers stabilize quickly once materially safe, the structural share dominates.',
    'Internalized suppression travels with the target after exit, raising effective suppression above the structural measure and meaning that no merely legal or economic remedy reaches most of the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the suppression bearing on the paying seats.').

omega_variable(
    martyrdom_premium_direction,
    'Does federal prosecution raise or lower the covenant''s hold on believers — does the martyrdom premium confirm the law''s divinity in believers'' eyes, or does sustained pressure erode it?',
    'Compare retention, defection, and endogamous-marriage rates across prosecution-intensity periods (the 1884-1890 crusade years versus dormancy intervals), controlling for cohort age.',
    'If martyrdom confirms, suppression_requirement keeps climbing and the arrangement hardens toward pure extraction with no internal brake; if it erodes, the series should turn down as the reading fails to reproduce itself in the next generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martyrdom_premium_direction, empirical, 'Direction of the martyrdom effect on the constraint''s persistence under external pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_immutable_cmd_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(emc_immutable_cmd_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(emc_immutable_cmd_tr_t20, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(emc_immutable_cmd_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(emc_immutable_cmd_tr_t40, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(emc_immutable_cmd_tr_t50, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(emc_immutable_cmd_tr_t60, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(emc_immutable_cmd_tr_t70, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 70, 0.45).

% Extraction over time
narrative_ontology:measurement(emc_immutable_cmd_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(emc_immutable_cmd_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(emc_immutable_cmd_be_t20, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(emc_immutable_cmd_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(emc_immutable_cmd_be_t40, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(emc_immutable_cmd_be_t50, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 50, 0.79).
narrative_ontology:measurement(emc_immutable_cmd_be_t60, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(emc_immutable_cmd_be_t70, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 70, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(emc_immutable_cmd_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(emc_immutable_cmd_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(emc_immutable_cmd_su_t20, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(emc_immutable_cmd_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(emc_immutable_cmd_su_t40, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(emc_immutable_cmd_su_t50, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 50, 0.84).
narrative_ontology:measurement(emc_immutable_cmd_su_t60, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 60, 0.87).
narrative_ontology:measurement(emc_immutable_cmd_su_t70, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 70, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Mormon plural marriage doctrine' decomposes into three structurally distinct constraints — one per reading of the eternal_marriage_covenant kernel — per the epsilon-invariance principle. The readings differ on exactly one structural element: whether the 1843 text carries a legitimate revision valve. This (immutable) reading has none, producing maximal rigidity, a martyrdom bind under federal pressure, and the widest victim set; the prophetic_override reading vests the valve in a living prophet; the temporal_accommodation reading splits doctrine from practice and suspends the latter. Epsilon differs across the family because the valve's presence or absence changes who bears costs and whether exit exists at any price. This upstream reading influences both siblings historically: the mainstream church's override-and-accommodation solution was constructed in response to the bind this reading created, and this reading persists as the fundamentalist reproach to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
