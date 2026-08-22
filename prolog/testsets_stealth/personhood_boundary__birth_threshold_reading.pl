% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_threshold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Boundary — Birth Threshold Reading
 *   domain: moral_philosophy/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the personhood_boundary kernel:
 *   the birth threshold, under which the birth event — and nothing else —
 *   fixes membership in the protected class. All born humans possess moral
 *   standing; killing any born human is homicide; no state, professional, or
 *   familial authority may exclude a born human from the class. The epsilon
 *   referent is the standing arrangement under contest — the birth-threshold
 *   boundary as this reading holds it — assessed by the reading's own lights:
 *   a rule that assigns standing unconditionally at a publicly verifiable
 *   event. On the expected structural delta: the delta's phrase 'all born
 *   infants in victim set' denotes the set of beings whose killing counts as
 *   wrongful killing — i.e., the protected class. In this schema the
 *   protected class maps to beneficiaries (the constraint's operation shields
 *   them; it does not extract from them), so newborn_infants and
 *   severely_disabled_born_persons appear under beneficiaries, and the only
 *   cost-bearing seat, desperate_parents_of_unwanted_newborns, appears under
 *   victims in the narrow sense of bearers of the rule's concentrated
 *   residual cost. The colloquial label 'when does personhood begin'
 *   decomposes into three structurally distinct constraints (this file plus
 *   the fitness-contingent and potential-based sibling files) with different
 *   victim-set extensions, different enforcement consequences, and different
 *   epsilon values; they are linked via network.affects_constraints, not
 *   merged here.
 *
 * KEY AGENTS:
 *   - newborn_infants: primary protected class (powerless/trapped) — standing assigned at birth, defended entirely by others
 *   - severely_disabled_born_persons: protected class at the dispute's edge (powerless/trapped) — the seats rival readings would strip
 *   - all_born_humans: universal reciprocal beneficiary (moderate/constrained) — every member was once covered as an infant and will be weak again at life's edges
 *   - desperate_parents_of_unwanted_newborns: concentrated cost-bearers (powerless/trapped) — no lawful exit from the care duty in the crisis window
 *   - state_legal_apparatus: administrator and enforcer (institutional/identity_locked) — collects the adjudication economy of a bright line it can no longer disown
 *   - forensic_medical_establishment: operational administrator (institutional/constrained) — holds the live-birth and death determinations the verbal line rides on
 *   - moral_philosophers_contesting_the_line: excluded contestants (moderate/mobile) — voice without vote over the criterion
 *   - human_rights_treaty_bodies: analytical monitor (institutional/analytical) — cross-jurisdiction view, no enforcement arm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.15).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.22).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.11).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, rope).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary — Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '4ad0be07-392f-4d72-a3e8-dafa989c1ca6').
narrative_ontology:cs_kernel_codification('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', formalized).
narrative_ontology:cs_authority_grounding('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', lineage).
narrative_ontology:cs_interpretation_layer_present('4ad0be07-392f-4d72-a3e8-dafa989c1ca6').
narrative_ontology:cs_reading_relation('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', foundational, birth_confers_unconditional_moral_standing).
narrative_ontology:cs_axiom_status(birth_confers_unconditional_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', birth_confers_unconditional_moral_standing, deontological).
narrative_ontology:cs_axiom('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', foundational, no_authority_may_exclude_born_humans).
narrative_ontology:cs_axiom_status(no_authority_may_exclude_born_humans, holdable).
narrative_ontology:cs_axiom_grounding('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', no_authority_may_exclude_born_humans, deontological).
narrative_ontology:cs_reference_frame('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', universal_birth_inclusion_baseline).
narrative_ontology:cs_drift_state('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ad0be07-392f-4d72-a3e8-dafa989c1ca6', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, all_born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, newborn_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, severely_disabled_born_persons).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, desperate_parents_of_unwanted_newborns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, desperate_parents_of_unwanted_newborns).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, forensic_medical_establishment).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_protection_doctrine).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, birthright_citizenship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enter the world already inside the protected class: the fact of birth, not health, parentage, or usefulness, settles their claim to life and legal personhood. They can do nothing to earn, keep, or assert this standing; every duty it creates falls on others, and its defense is entirely third-party — birth registration, homicide law, and the refusal of any authority to entertain removal.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, newborn_infants, beneficiary,
    powerless, biographical, trapped, global).

% Born with conditions that make dependence permanent or lifelong. Under this reading their standing is identical to any other born person's, and no tribunal, physician, or official may reclassify them out of it. What flows to them is unconditional inclusion; what is asked of others is care they cannot reciprocate. Their families' exhaustion, not their status, is the pressure point at which rivals of this reading aim.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, severely_disabled_born_persons, beneficiary,
    powerless, biographical, trapped, global).

% Every living human stands inside the line at all times, and the coverage is reciprocal: each member was once a dependent infant covered by the same rule they now help uphold, and each will be weak again at its edges through illness and age. Membership cannot be resigned, traded, or transferred; the only exit is death, which the rule itself regulates.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, all_born_humans, beneficiary,
    moderate, generational, constrained, global).

% Bear the rule's sharpest edge: a parent unable to feed, house, or survive alongside a newborn has no lawful exit — exposure, abandonment to death, and informal disposal are crimes, and surrender to state care is the only door, with its own delays and stigma. The same parents stood inside the protected class as infants and will stand inside it again as adults; the cost lands on them in the narrow window when the duty is heaviest and their resources lowest.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, desperate_parents_of_unwanted_newborns, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, desperate_parents_of_unwanted_newborns, beneficiary).

% Legislatures, courts, and registries administer the line: they define live birth, register every arrival, prosecute killings without inquiry into the victim's capacities, and decline legislation that would sort born humans by worth. The bright edge spares them per-case hearings on who counts, so adjudication cost drops to near zero, and the equal-protection commitment has become part of the state's own legitimacy story — stepping back from it would wound the institution itself.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).

% Pediatricians, coroners, and registrars hold the operational pen: their certifications decide when the line is crossed in both directions — live birth and death — and their professional authority grows with custody of that determination. Neonatology keeps moving the practical facts beneath the fixed verbal line, and the profession manages the gap case by case without reopening the rule.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, forensic_medical_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, forensic_medical_establishment, beneficiary).

% Publishers and teachers of rival thresholds — demonstrated fitness, rational-agency potential — argue that the birth line over-includes or rests on an arbitrary event. Their arguments circulate freely in journals and classrooms but carry no vote: legislatures and courts do not treat them as actionable, and several jurisdictions have codified the opposite. Exit is easy — they can write anywhere — but influence over the rule itself is closed to them.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, moral_philosophers_contesting_the_line, excluded,
    moderate, generational, mobile, global).

% Treaty committees and monitoring organs review state compliance with the unconditional-inclusion standard, take reports from governments and advocacy groups, and name deviations in public findings. They see the whole structure across jurisdictions but command no police; their instruments are documentation and reputational pressure.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, state_legal_apparatus).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the question 'who counts' with a single publicly observable event: birth. Any registrar, court, or neighbor can verify membership without judging worth, capacity, or potential, so homicide protection, registration, inheritance, and equal treatment run at near-zero adjudication cost, and no agent holds discretion over another's standing.
% TRANSFER_FUNCTION: Moves unconditional protection toward every born human, funded by a universal surrender: each parent, community, and state gives up the liberty to kill, expose, or sort born humans. The residual concentrated cost — sustaining an infant one cannot afford — lands on the least-resourced caregivers wherever support institutions are thin.
% ABSENT_VOICES: Newborns — the class the line defines — cannot speak and are represented only through proxies. The mothers whom history criminalized for exposure were silenced twice: first by poverty, then by a record written entirely by their prosecutors. Rival-threshold theorists are heard but hold no vote; and the infants who died under pre-line discretion left no testimony at all.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen the question the line closed: fitness boards, paternal discretion, and state sorting would return, homicide law would fragment into case-by-case capacity inquiries, and the equality architecture — civil rights, disability protection, citizenship at birth — built on unconditional personhood would lose its load-bearing wall. Every registry, statute, and constitutional clause touching birth would need rewriting.
% FOUNDING_PROBLEM: Under pre-line regimes — Roman exposure rights, Spartan selection, comparable customs elsewhere — an infant's survival hung on a father's or council's judgment of fitness, and the law protected no born human until discretion admitted them. The line was built to delete that discretion: to make survival independent of anyone's verdict on worth.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: classical historians attest the exposure regimes the line abolished; the record of twentieth-century eugenic boards, and survivors' testimony, attest the discretion's attempted returns; disability-rights organizations — whose members are precisely the class rival readings would strip — attest the line's continuing necessity. Rival-reading proponents corroborate liveness from the opposite direction, arguing the line over-includes; that the underlying problem keeps generating disputes is the one point on which all parties agree.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).
:- end_tests(personhood_boundary__birth_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end) because the arrangement's net flow is protective: what it takes (the liberty to kill, expose, or sort born humans) is surrendered by all and converts into security received by all, with the small residual above the identity-coordination floor reflecting the concentrated care burden on the least-resourced caregivers. Suppression is low-moderate (0.22) because the line persists overwhelmingly through internalized consensus; criminal law backs the conduct rule, but no ongoing campaign restrains a resistant population — resistance is philosophical, not behavioral. Theater is minimal (0.11) and rises only gently with ceremonial reaffirmation (anniversary declarations, commemorative instruments) around a stable functional core. Accessibility_collapse is 0.45: rival thresholds (quickening, viability, fitness, potential) existed historically and remain live in the sibling files, so alternatives are rejected within this tradition but not erased from the possibility space. Resistance is 0.42: sustained academic contest plus the historical persistence of infanticide and eugenic sorting against the rule, short of mass movement. The three measurement series run on ONE shared grid (T0-T75 at 15-unit steps, approximately 1948-2023 at one unit per year) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately because this story tracks enforcement-capacity change: active enforcement against exposure and eugenic-sorting residues was heavy at mid-century and has normalized downward as consensus internalized — a falling trajectory modeling enforcement maturation into redundancy, not decay of the norm. Base extractiveness drifts gently upward as neonatal medicine extends survival and thereby concentrates the care-burden asymmetry on fragile-infant families.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the protected seats (newborns, severely disabled persons, the general born class) the line is a shield: pure assignment of standing, zero adjudication, no discretion held over them. From the desperate-parent seat the same line is a wall with one door: it removes every exit their pre-line counterparts had, at the exact moment they can least bear the duty. From the state seat it is a cheap bright edge — near-zero per-case cost — fused with an legitimacy commitment the institution can no longer renounce without self-wound. From the excluded philosopher seat it is an arbitrary criterion holding office without argument. From the treaty-monitor seat it is a standard whose variance across jurisdictions is the finding. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the protected seats to the beneficiary end: newborn_infants and severely_disabled_born_persons are trapped recipients of pure subsidy (d near 0.0), and all_born_humans sits near-symmetric-with-mild-gain since every member both upholds and is upheld by the line. The single victim declaration, desperate_parents_of_unwanted_newborns, places that seat toward the target end (high d) — they bear the rule's binding cost — while their secondary beneficiary role records the reciprocity (they were covered as infants, will be covered again). The delta's 'victim set' language maps to the protected class, hence beneficiaries here; the schema's victims array means cost-bearers under the arrangement, and the only identifiable such seat is the desperate-parent class. A directionality override is authored for the institutional power atom (d = 0.18) because the structural derivation cannot see that all three institutional seats in this story are near-symmetric administrators with mild net gain — the state collects adjudication economy but pays enforcement and bears identity-lock; the medical establishment collects professional authority but absorbs the neonatology gap; the treaty bodies collect nothing and spend monitoring effort. A canonical institutional fallback would risk reading these seats as extractive administrators, which the situations do not support.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — discretionary verdicts over infant survival — is live: the discretion the line deleted keeps attempting returns (eugenic boards historically, fitness-and-potential arguments contemporaneously), so mandatrophy is not resolved and no sunset is declared. The classification guards against two symmetrical mislabels. First, the snare-flavored misread: the line does coerce would-be violators, but the coerced 'interest' (killing or sorting the unprotected) is not a legitimate competing interest, and no seat receives the surrendered liberty as gain — the arrangement coordinates rather than extracts. Second, the mountain-flavored misread: the line feels self-evident, but it is a constructed commitment — coherent rival readings exist and are held — so emerges_naturally stays false and the rule's persistence must be explained by coordination value, not naturality. Finally, the falling suppression_requirement series must not be read as piton drift: the function is intact, theater is negligible, and the omega normalization_vs_atrophy routes the ambiguity to investigation rather than letting the trajectory silently date a transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (birth_threshold_reading) of the personhood_boundary kernel; what would adoption of a sibling reading change structurally?',
    'Comparative structural analysis across the three sibling files: each sibling''s victim-set extension, exclusion-authority provisions, and enforcement consequences are authored independently; resolution consists of holding all three files fixed and comparing their computed structures, not averaging them.',
    'Adopting fitness_contingent_reading would remove pre-fitness born infants from the protected class and license exclusion authority; adopting potential_based_reading would remove severely disabled born infants. This file''s own classification is unchanged either way, but network contamination analysis must treat the siblings as distinct constraints with different victim sets, not variants of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the personhood_boundary kernel; sibling adoption shrinks the protected class and reintroduces exclusion discretion.').

omega_variable(
    care_burden_extraction_location,
    'Does the concentrated cost borne by caregivers of unsupportable infants constitute extraction operating through the birth line itself, or through adjacent missing support institutions?',
    'Cross-jurisdictional comparison of regimes that pair the unconditional line with robust infant-support systems versus those that pair it with thin support: if caregiver hardship tracks the welfare gap rather than the line, the cost is external to this constraint; if hardship persists at equal support levels, the line itself carries it.',
    'If the burden runs through the line, the constraint migrates toward a hybrid with a genuine coordination function plus asymmetric concentrated cost; if it runs through adjacent institutions, this file remains a clean coordination arrangement and the burden belongs in separate support-institution stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_burden_extraction_location, empirical, 'Whether the desperate-parent cost is attributable to the birth line or to the absence of companion welfare arrangements.').

omega_variable(
    birth_event_moral_relevance,
    'Is birth a morally relevant discontinuity — separation from the maternal organism, independent respiration, visible entry into the social world — or merely an administratively convenient proxy for capacities that develop continuously?',
    'Convergence of developmental neuroscience on the moral significance of the birth transition with sustained philosophical analysis of whether any event-marked criterion can bear unconditional standing; the sibling files press the proxy reading from the capacity side.',
    'If birth is mere proxy, the line''s stability rests on administrative convenience rather than moral fact, strengthening sibling pressure and raising long-run revision risk; if the event carries genuine moral salience, this reading''s foundation hardens and the measured low extraction reflects a well-founded arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(birth_event_moral_relevance, conceptual, 'Whether the threshold event is morally load-bearing or an administrative proxy for gradual capacity development.').

omega_variable(
    normalization_vs_atrophy,
    'Is the falling suppression_requirement series healthy normalization — internalized consensus making active enforcement redundant — or early atrophy, with enforcement capacity decaying ahead of any consensus shift and leaving the line unguarded at its edges?',
    'Probe institutional response latency and vigor on edge-case violations (neonaticide prosecutions, neonatal-triage disputes, registration refusals): brisk, uniform responses indicate normalization; slow, inconsistent, or declined responses indicate atrophy.',
    'The atrophy reading would flag drift toward inertial maintenance — a rule kept verbally while its enforcement substance thins; the normalization reading confirms the arrangement''s stability and validates the low suppression figure as structural rather than vestigial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_vs_atrophy, empirical, 'Whether declining enforcement intensity reflects consolidated consensus or eroding guardianship of the line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t15, personhood_boundary__birth_threshold_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement_basis(pers_tr_t15, observed).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__birth_threshold_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(pers_tr_t30, observed).
narrative_ontology:measurement(pers_tr_t45, personhood_boundary__birth_threshold_reading, theater_ratio, 45, 0.09).
narrative_ontology:measurement_basis(pers_tr_t45, observed).
narrative_ontology:measurement(pers_tr_t60, personhood_boundary__birth_threshold_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(pers_tr_t60, observed).
narrative_ontology:measurement(pers_tr_t75, personhood_boundary__birth_threshold_reading, theater_ratio, 75, 0.11).
narrative_ontology:measurement_basis(pers_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t15, personhood_boundary__birth_threshold_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement_basis(pers_be_t15, observed).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__birth_threshold_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement_basis(pers_be_t30, observed).
narrative_ontology:measurement(pers_be_t45, personhood_boundary__birth_threshold_reading, base_extractiveness, 45, 0.13).
narrative_ontology:measurement_basis(pers_be_t45, observed).
narrative_ontology:measurement(pers_be_t60, personhood_boundary__birth_threshold_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement_basis(pers_be_t60, observed).
narrative_ontology:measurement(pers_be_t75, personhood_boundary__birth_threshold_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement_basis(pers_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t15, personhood_boundary__birth_threshold_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement_basis(pers_su_t15, observed).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__birth_threshold_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement_basis(pers_su_t30, observed).
narrative_ontology:measurement(pers_su_t45, personhood_boundary__birth_threshold_reading, suppression_requirement, 45, 0.26).
narrative_ontology:measurement_basis(pers_su_t45, observed).
narrative_ontology:measurement(pers_su_t60, personhood_boundary__birth_threshold_reading, suppression_requirement, 60, 0.24).
narrative_ontology:measurement_basis(pers_su_t60, observed).
narrative_ontology:measurement(pers_su_t75, personhood_boundary__birth_threshold_reading, suppression_requirement, 75, 0.22).
narrative_ontology:measurement_basis(pers_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'when does personhood begin'. The single natural-language concept covers three structurally distinct claims with different victim-set extensions, different exclusion-authority provisions, and different epsilon values: this file (birth sufficient and unrebuttable; maximal inclusion; no exclusion authority), personhood_boundary__fitness_contingent_reading (standing conditional on demonstrated fitness), and personhood_boundary__potential_based_reading (standing conditional on rational-agency potential). Each file links the other two via affects_constraints. Upstream/downstream structure: the birth reading is upstream in law — codified earliest, cited as the settled baseline that sibling proposals must amend — while the siblings exert downstream theoretical pressure on it; the upstream claim's stability is routinely cited as evidence against the downstream revisions, which is why contamination propagates from this file toward the siblings rather than only the reverse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
