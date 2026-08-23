% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending (Sanctity Reading of End-of-Life Decision Authority)
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   The sanctity reading instantiates the end_of_life_decision_authority
 *   kernel as an absolute: human life possesses intrinsic value independent
 *   of individual will, so intentional life-ending is impermissible
 *   regardless of consent, suffering, or process. The reading is codified
 *   across most jurisdictions as criminal prohibitions on euthanasia and
 *   assisted dying, reinforced by professional ethics codes binding
 *   physicians to a healer-only role. It is one of three live readings of the
 *   kernel; the autonomy reading and the vulnerability_protection reading are
 *   separate constraints with different beneficiary and victim structures,
 *   authored as sibling stories and linked through the network. The ε
 *   referent here is the standing prohibition arrangement itself, assessed by
 *   this reading's own lights: the reading holds that nothing owed to anyone
 *   is taken, while its own structural analysis concedes that the arrangement
 *   imposes real costs on competent suffering individuals whose
 *   self-determination is overridden and whose suffering is prolonged — the
 *   reading's declared structural delta names this 'individual suffering
 *   externalized,' and notes that the pressured-vulnerable, who sit in THIS
 *   reading's protected set, would enter the victim set under the autonomy
 *   reading. The claim/metric gap is deliberate: the reading CLAIMS mountain
 *   (a moral law that would hold regardless of who enforces it) while the
 *   authored metrics describe an actively enforced norm with declared
 *   beneficiaries, real resistance, and slowly decaying enforcement — that
 *   divergence is what the false-summit machinery exists to measure. Interval
 *   mapping: T=0 approximates the mid-1970s onset of the modern debate
 *   (Quinlan era, hospice movement); T=48 approximates the present.
 *
 * KEY AGENTS:
 *   - legislatures_and_courts: agenda-setter (institutional/constrained) — enacts and adjudicates the prohibition
 *   - medical_professional_bodies: agenda-setter and beneficiary (institutional/constrained) — writes and enforces the healer-only ethics code
 *   - religious_institutions: beneficiary (organized/mobile) — receives civil-law backing for doctrinal teaching
 *   - pressured_vulnerable_patients: protected beneficiary (powerless/trapped) — the class the prohibition shields from implicit pressure
 *   - competent_terminally_ill_patients: primary target (powerless/trapped) — bears prolonged suffering and foregone self-determination
 *   - assisted_dying_physicians: target (organized/constrained) — bears prosecution and license loss for compassionate assistance
 *   - bereaved_families_of_suffering_deaths: excluded voice (moderate/mobile) — holds the most vivid testimony, holds no drafting seat
 *   - bioethics_commissions: analytical observer (institutional/analytical) — produces the evidentiary record both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.3).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.65).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, mountain).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending (Sanctity Reading of End-of-Life Decision Authority)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).
domain_priors:emerges_naturally(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '929ad8a3-b028-4381-ab3a-f884ab276672').
narrative_ontology:cs_kernel_codification('929ad8a3-b028-4381-ab3a-f884ab276672', formalized).
narrative_ontology:cs_authority_grounding('929ad8a3-b028-4381-ab3a-f884ab276672', lineage).
narrative_ontology:cs_interpretation_layer_present('929ad8a3-b028-4381-ab3a-f884ab276672').
narrative_ontology:cs_reading_relation('929ad8a3-b028-4381-ab3a-f884ab276672', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('929ad8a3-b028-4381-ab3a-f884ab276672', end_of_life_decision_authority__vulnerability_protection_reading, forecloses).
narrative_ontology:cs_axiom('929ad8a3-b028-4381-ab3a-f884ab276672', foundational, life_value_independent_of_individual_will).
narrative_ontology:cs_axiom_status(life_value_independent_of_individual_will, holdable).
narrative_ontology:cs_axiom_grounding('929ad8a3-b028-4381-ab3a-f884ab276672', life_value_independent_of_individual_will, deontological).
narrative_ontology:cs_axiom('929ad8a3-b028-4381-ab3a-f884ab276672', secondary, physician_never_intentionally_kills).
narrative_ontology:cs_axiom_status(physician_never_intentionally_kills, holdable).
narrative_ontology:cs_axiom_grounding('929ad8a3-b028-4381-ab3a-f884ab276672', physician_never_intentionally_kills, conventional).
narrative_ontology:cs_reference_frame('929ad8a3-b028-4381-ab3a-f884ab276672', inviolable_life_sanctity_norm).
narrative_ontology:cs_drift_state('929ad8a3-b028-4381-ab3a-f884ab276672', contemporary_permissive_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('929ad8a3-b028-4381-ab3a-f884ab276672', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_professional_bodies).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, competent_terminally_ill_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, assisted_dying_physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, hippocratic_non_killing_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain the criminal prohibitions on euthanasia and assisted dying, and adjudicate constitutional challenges to them. Individual legislators and judges face sustained lobbying from religious institutions, medical associations, and disability organizations on one side and right-to-die campaigns and bereaved families on the other; a legislator who champions repeal bears electoral risk, so most hold the line or carve out narrow defenses (double effect, treatment withdrawal) through judicial interpretation rather than statute.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Write and enforce the ethics codes that define the physician's role at the end of life, and discipline members who cross them. Their members hold a clearly bounded professional identity — healers who never intentionally kill — which protects patient trust and shields the profession from complicity disputes; the bodies themselves hold institutional authority as arbiters of that boundary. Exit would mean abandoning the code tradition the profession is built on.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_professional_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, medical_professional_bodies, beneficiary).

% Teach that life is a gift held on trust rather than owned, and mobilize politically against any legalization of intentional death. The criminal prohibition gives their moral teaching the backing of civil law at no cost to themselves, and their own members are rarely the ones whose deaths are at issue. They can carry the commitment across borders and would continue regardless of any single jurisdiction's law.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions, beneficiary,
    organized, generational, mobile, global).

% Elderly, disabled, and economically dependent people whose continued lives are expensive or burdensome to others. The prohibition keeps intentional death outside the menu of options their families, caregivers, insurers, and institutions may offer or imply. They cannot opt out of this protection, and relocating to a permissive jurisdiction is beyond most of them. Their stake is in the norm holding, though they have little organized voice in how it is defended.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, beneficiary,
    powerless, biographical, trapped, national).

% Competent adults with irremediable suffering who want to determine the manner and timing of their own deaths and are denied legal assistance. They bear the arrangement's full cost: prolonged suffering, foregone self-determination, and in some cases violent or lonely deaths. Near end of life they generally cannot relocate to permissive jurisdictions; the lawful options left to them — refusing treatment, palliative sedation, stopping eating and drinking — are each slower or more burdensome than what they asked for.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, competent_terminally_ill_patients, payer,
    powerless, immediate, trapped, national).

% Physicians who conclude that assisting a dying patient's death is compassionate care and who act or speak accordingly. They face license revocation, prosecution, and professional ostracism, and some have been convicted. Their exits are career conversion or silence; most choose silence, which is why enforcement volume stays low even where the rule's legitimacy is widely questioned inside the profession.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, assisted_dying_physicians, payer,
    organized, biographical, constrained, national).

% Families who watched a member die in prolonged agony under the prohibition and who campaign for legal access in that member's name. They hold the most vivid testimony in the debate but no formal seat: legislative committees hear them as witnesses, while the drafting tables are occupied by the professional, religious, and disability organizations.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bereaved_families_of_suffering_deaths, excluded,
    moderate, biographical, mobile, national).

% Government-commissioned and academic bodies that hold hearings, review safeguard data from permissive jurisdictions, and issue reports on end-of-life law. They bear no direct cost and collect no direct benefit; their reports supply the evidentiary record that both sides cite.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single society-wide rule at the end of life: no one may intentionally end another's life, and physicians never kill. This addresses a real collective-action problem — dependent people cannot individually negotiate immunity from the pressure that cheap, available death would place on families, insurers, and care institutions, and patients cannot individually verify that their healer's judgment is untainted by the option of killing. One uniform boundary is set once for everyone instead of renegotiated case by case.
% TRANSFER_FUNCTION: Moves decision authority and its costs rather than goods: it withholds end-of-life decision authority from competent individuals and their physicians and holds it with the state and the profession, while allocating the arrangement's costs — prolonged suffering, foregone self-determination, criminal exposure for assisting physicians — to competent dying individuals and their would-be assistants, and its benefits — protection from implicit pressure, doctrinal vindication, professional role clarity — to the vulnerable class, religious institutions, and the profession.
% ABSENT_VOICES: Competent dying patients are the debate's rhetorical center but hold no seat: they die before the law changes, and none sits on drafting committees or ethics-code boards. Bereaved families of suffering deaths testify but do not draft. Early-stage degenerative patients who will face the choice are unrepresented. The deliberating bodies — legislatures, professional bodies, religious institutions, disability organizations — are all composed of people who expect to outlive the decision.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, assisted dying would emerge quickly where demand and physician willingness exist, medical roles would split into killing and non-killing tracks, insurers and health systems would face immediate incentive questions about expensive dependent lives, religious institutions would mobilize against the new legality, and the pressured-vulnerable would face a new ambient risk their advocates would rush to litigate. End-of-life practice, professional identity, and family decision dynamics would all reorganize around the new permission.
% FOUNDING_PROBLEM: To keep intentional killing from becoming an accepted instrument: the norm descends from the Hippocratic separation of healing from killing and from religious teaching on the inviolability of life, and was re-codified in the twentieth century in explicit response to state-organized killing of the disabled and the eugenics movement — the commitment that no one's death may be decided as a policy matter by family, physician, insurer, or state.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations — outside the religious and professional beneficiary set — corroborate that the pressure problem is live, citing permissive-jurisdiction findings on implicit coercion and care-cost pressure; palliative-medicine literature corroborates both the protective function and the suffering cost. Against the live reading, constitutional litigants, right-to-die organizations, and bereaved-family campaigns attest from outside the beneficiary set that the founding problem is substantially historical and that the arrangement now chiefly imposes suffering. Both directions carry extra-beneficiary attestation; neither genealogy is self-serving only.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_decision_authority__sanctity_reading),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.30) because it is reading-indexed: by the sanctity reading's own lights nothing owed is taken — life was never the individual's to end — yet the reading concedes real imposed costs on competent suffering individuals (prolonged suffering, overridden self-determination) whose relief is foreclosed rather than transferred, and concedes the suffering is externalized onto individuals rather than internalized by the system. Suppression is 0.65: the arrangement is held by criminal law and professional discipline — real coercion of the act — though treatment refusal, palliative sedation, and voluntary stopping of eating and drinking remain lawful, so the act-space is narrowed, not closed. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, in the engine, by directionality and scope. Theater is low (0.20): the prohibition functionally binds — prosecutions occur, licenses are revoked — but a growing share of maintenance is selective (non-prosecution policies, tolerated member advocacy, symbolic reaffirmation), which is why theater rises slowly across the interval while formal enforcement decays. Accessibility collapse is 0.50: alternatives persist and are partly workable, with jurisdictional arbitrage available to the wealthy. Resistance is 0.60: organized right-to-die movements, constitutional litigation, legislative campaigns, and physician civil disobedience are sustained and partly effective. The measurement series run on one shared time grid — every tracked metric is authored at every examined time point — and the final values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the competent_terminally_ill_patients seat, the arrangement is imposed suffering: the state and profession hold an authority over the patient's own death that the patient is denied, and the cost is externalized onto the person dying. From the religious_institutions and medical_professional_bodies seats, the same structure is protective moral order: a boundary that keeps killing out of the healing relationship and keeps pressure off the dependent. From the pressured_vulnerable_patients seat it is survival infrastructure. The engine computes per-seat types from power, exit, and role data; the authored claim does not adjudicate between these readings of the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats derive low directionality: religious_institutions (organized, mobile) and pressured_vulnerable_patients (powerless, trapped) are subsidized by the arrangement rather than targeted by it — note that the vulnerable class's trapped exit does NOT amplify their effective extraction, because trap only deepens extraction for targets; for beneficiaries it merely locks in the protection. medical_professional_bodies and legislatures_and_courts sit at the agenda-setting end, deriving near-beneficiary directionality with a capture caveat: professional bodies both administer and collect (role clarity, arbitership), which is why they carry a secondary beneficiary role rather than a directionality override — the structural data already yields the correct relationship. Target seats derive high directionality: competent_terminally_ill_patients (powerless, trapped, immediate horizon) sit nearest the full-target end, and assisted_dying_physicians (organized, constrained) somewhat below them. National scope applies moderate verification-difficulty amplification to the targets' effective extraction. No directionality overrides are needed: the beneficiary/victim declarations plus power and exit atoms produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification machinery prevents mislabeling in both directions. Without the coordination function — the genuine protection of the pressured-vulnerable and the healer-trust boundary — the prohibition would read as pure extraction from the dying; without the extraction — the imposed suffering and denied self-determination of competent individuals — it would read as a pure coordination good or natural law. The declared beneficiaries on a mountain claim route the story through false-summit evaluation: if the metric profile had been mountain-consistent, the engine would reclassify to the tangled-rope override target, preserving both the coordination function and the asymmetric cost-bearing in one type. Mandatrophy is NOT declared resolved: the founding problem (keeping intentional killing from becoming an accepted instrument) is contested-live, not dead — disability-rights corroboration from outside the beneficiary set attests the pressure problem is real in permissive jurisdictions, while autonomy-side corroboration attests the eugenic-era problem is historical. The arrangement's function is still substantially performed, so no zombie flag is authored. The rising theater_ratio and falling suppression_requirement series are the drift signature to watch: if enforcement decays further while the formal rule persists, the arrangement drifts toward inertial, selectively maintained operation in some jurisdictions — a trajectory the enforcement_decay_trajectory omega tracks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the intrinsic value of life a genuine natural moral law that would hold regardless of who enforces it, or a constructed norm maintained by criminal law and professional discipline that benefits identifiable parties — religious institutions, the medical profession, and the protected vulnerable class?',
    'Comparative institutional analysis across jurisdictions where the prohibition collapsed: whether the harms to the vulnerable that this reading predicts actually materialized, combined with meta-ethical analysis of whether the value claim has standing independent of its enforcement history.',
    'If constructed, the mountain claim is a false summit and reclassification toward tangled_rope stands, with the declared beneficiaries as the extraction''s institutional recipients of vindication; if natural law, the declared beneficiaries are incidental and the arrangement approaches a genuine mountain from every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Whether the sanctity prohibition is natural law or enforced construction (required FSM ambiguity documentation for a mountain claim with declared beneficiaries).').

omega_variable(
    vulnerable_protection_efficacy,
    'Does the absolute prohibition actually protect the pressured-vulnerable better than checkpointed permissive regimes would, or do safeguards in permissive jurisdictions achieve equivalent protection?',
    'Longitudinal comparison of vulnerable-group death patterns, implicit-pressure findings, and care-cost pressure indicators in permissive jurisdictions (Oregon, Netherlands, Belgium, Canada) against prohibition jurisdictions matched for palliative-care access.',
    'Equivalent protection under safeguards would make the prohibition over-inclusive — its imposed costs would not be necessary for its protective benefit, shifting the structure toward pure extraction; demonstrable safeguard failure would confirm the coordination function as load-bearing and the extraction as the price of the protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_protection_efficacy, empirical, 'Whether the prohibition''s protective function requires the absolute form.').

omega_variable(
    suffering_cost_status,
    'Is the cost the arrangement imposes on competent suffering individuals — prolonged suffering and foregone self-determination — extraction of something owed to them, or a refusal to transfer something no one owes?',
    'Not resolvable by data alone: it turns on whether a right to assisted death exists, which is the kernel contest itself. It resolves only by adopting a sibling reading or by a meta-ethical argument both sides accept; tracked structurally via the sibling stories.',
    'If extraction, the reading-indexed extractiveness is understated and the arrangement is extractive by design; if refusal-to-transfer, extractiveness approaches zero and the arrangement is protective with tragic side costs. The entire divergence between this reading and the autonomy reading lives in this omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suffering_cost_status, conceptual, 'The conceptual crux dividing the sanctity reading from its siblings.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is the sanctity reading of the end_of_life_decision_authority kernel: how would the sibling readings restructure the beneficiary and victim sets, and where exactly is the disagreement located?',
    'Authored in the sibling stories (autonomy_reading, vulnerability_protection_reading); within this reading it is not resolvable. The disagreement is located in whether individual will can generate death-authority at all, and each reading answers it differently.',
    'The autonomy reading moves the pressured-vulnerable out of the protected set and into the victim set, dissolves the healer-only physician role, and internalizes individual suffering as a cost the individual may choose to end; the vulnerability_protection reading converts the absolute prohibition into checkpointed permission, redistributing authority across institutions. Adopting either sibling changes the beneficiary/victim structure and flips the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints, not positions inside this one.').

omega_variable(
    enforcement_decay_trajectory,
    'Is the declining enforcement intensity — falling prosecution volume, prosecutorial discretion policies, professional-body code softening — a transition toward repeal in which the arrangement will be replaced, or decay toward a formally maintained, selectively enforced residue?',
    'Track prosecution rates, professional ethics-code revisions, and legislative outcomes across prohibition jurisdictions over the coming decade.',
    'Continued decay with formal maintenance would drift the arrangement toward inertial, theatrical operation in some jurisdictions and strengthen the piton-flavored reading of the rising theater series; legislative reversal would confirm the arrangement as an actively contested live norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether enforcement decay ends in repeal or in hollow maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__sanctity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__sanctity_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__sanctity_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__sanctity_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(end__tr_t48, end_of_life_decision_authority__sanctity_reading, theater_ratio, 48, 0.2).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(end__be_t48, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 48, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(end__su_t48, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 48, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'end-of-life decision authority' decomposes into three structurally distinct constraints — one per live reading of the kernel. This story (sanctity_reading) authors the prohibition arrangement: beneficiaries are the pressured-vulnerable, religious institutions, and the profession; victims are the denied competent individual and the assisting physician. The autonomy_reading story authors the permissive arrangement with the inverted victim structure (the pressured-vulnerable enter the victim set; individual suffering is internalized as choosable). The vulnerability_protection_reading story authors the checkpointed arrangement, which distributes rather than denies or individualizes death-authority. The ε values differ across the family because each reading assesses its own referent by its own lights; the readings are linked, not merged. This story forecloses both siblings within any single framework, which is why the kernel remains a live three-way contest across jurisdictions rather than a settled rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
