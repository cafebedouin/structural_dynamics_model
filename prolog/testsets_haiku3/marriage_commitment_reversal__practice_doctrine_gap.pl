% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Doctrine-Practice Gap (1890-1904)
 *   domain: religious/institutional/political-theology
 *
 * SUMMARY:
 *   Between 1890 and 1904, a major religious institution facing federal legal
 *   pressure on plural marriage made a structural choice: preserve the
 *   doctrinal principle (Section 132, declaring celestial marriage eternal
 *   and binding) while suspending the public practice (directing bishops to
 *   cease performing plural marriages). This generated a doctrine-practice
 *   gap: the commitment that doctrine says is binding is administratively
 *   suspended in the jurisdictions where it would be illegal. General members
 *   experience a contradiction they cannot resolve without exiting their
 *   identity framework; fundamentalist schismatics interpret the gap as
 *   institutional apostasy; institutional leadership gains flexibility to
 *   interpret which marriages are 'really' binding (sealed records, closed
 *   doctrine) and which are 'suspended' (public, deniable). The
 *   extractiveness comes from the leadership's capture of interpretive
 *   authority — they alone decide which doctrine applies where, dissolving
 *   member clarity about what the institution actually commits them to.
 *
 * KEY AGENTS:
 *   - Institutional leadership: controls doctrine-practice boundary and interpretive discretion; benefits from flexibility without formal doctrinal abandonment.
 *   - General membership: identity-locked into faith; experience cognitive dissonance between taught doctrine and suspended practice.
 *   - Fundamentalist schismatics: interpret the gap as apostasy; exit the institution but face social/kinship costs; feel vindicated that doctrine was never really abandoned, only hidden.
 *   - Affected women: trapped in plural marriages formalized before reversal; legally liminal (spiritually sealed, civilly disappeared).
 *   - Federal authorities: apply external pressure; observe the gap as evasion disguised as compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.79).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Doctrine-Practice Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious/institutional/political-theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, 'cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4').
narrative_ontology:cs_kernel_codification('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', fixed_text).
narrative_ontology:cs_authority_grounding('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', extraction).
narrative_ontology:cs_interpretation_layer_present('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4').
narrative_ontology:cs_reading_relation('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', marriage_commitment_reversal__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', foundational, doctrinal_preservation_despite_practice_suspension).
narrative_ontology:cs_axiom_status(doctrinal_preservation_despite_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', doctrinal_preservation_despite_practice_suspension, instrumental).
narrative_ontology:cs_axiom('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', secondary, institutional_authority_control_of_interpretation).
narrative_ontology:cs_axiom_status(institutional_authority_control_of_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', institutional_authority_control_of_interpretation, conventional).
narrative_ontology:cs_reference_frame('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', section_132_eternally_binding).
narrative_ontology:cs_drift_state('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', post_federal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cfe94390-c8b9-438e-ab5d-c6d7bf72c7a4', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schismatics).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, affected_women).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, divine_revelation_flexibility).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preserves Section 132 doctrine (celestial marriage eternal, binding) in official canon while directing bishops to cease officiating plural marriages in public jurisdictions. Controls the interpretive framework and enforcement discretion: which marriages are 'celestial' and recorded in sealed records, which are publicly denied. Collects institutional legitimacy through claimed doctrinal consistency while gaining practical compliance with federal pressure through suspended enforcement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Invested in the doctrine that marriage commitment is eternal and divinely mandated. Now experience a contradiction: the same institution that taught Section 132's permanence has suspended it in practice while keeping the doctrine officially intact. They cannot exit the faith without severing personal identity and community ties; they cannot stay without managing cognitive dissonance about whether the doctrine they were taught is actually in force.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    organized, biographical, identity_locked, national).

% Interpret Section 132 as absolute and divine, requiring plural marriage as restoration doctrine. They experience the institutional reversal as apostasy and the doctrine-practice gap as proof of institutional capitulation. Their exit is costly (forming separate communities, severing kinship, losing institutional infrastructure) but their constraints are less identity-locked than general membership because they have already separated their spiritual allegiance from institutional authority.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schismatics, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schismatics, excluded).

% In plural marriages formalized before the reversal, caught in legal limbo: the institution claims the marriages are 'sealed' (spiritually real, eternally binding per Section 132) while denying them publicly (civil recognition suspended, new public ceremonies forbidden). Their legal status, property rights, and spousal claims become contestable; they are told their commitments are divine while being treated as disappeared in public records.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, affected_women, payer,
    powerless, biographical, trapped, national).

% Applied legal and electoral pressure (anti-polygamy statutes, disenfranchisement, threat of corporate dissolution) that prompted the institutional reversal. They observe the doctrine-practice gap and interpret it as evasion: the institution retained plural-marriage doctrine while appearing to comply with law, maintaining the commitment's theological force in sealed records and private theology while suspending public practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_authorities, observer,
    institutional, generational, analytical, national).

% Scholars, journalists, and rival institutions observing the contradiction. They see institutional leadership as navigating between incommensurable claims: staying doctrinally pure (Section 132 remains) while achieving practical compliance (practice suspended). The doctrine-practice gap itself becomes evidence of institutional sophistry or adaptive genius, depending on the observer's frame.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, peripheral_observers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides doctrinal coherence for a faith community's understanding of marriage as eternal commitment. The coordination problem was: how does a religious movement maintain its founding theology while adapting to legal and political pressure that makes the theology's public practice illegal?
% TRANSFER_FUNCTION: Transfers institutional authority from doctrinal transparency to interpretive discretion: the gap between preserved doctrine and suspended practice gives institutional leadership unilateral power to declare which commitments are 'real' (sealed, eternal) and which are 'suspended' (public, mutable). This transfers legitimacy-granting power from members (who believed doctrine was binding) to authorities (who now control which doctrine applies in which jurisdiction).
% ABSENT_VOICES: Federal authorities are heard as imposing constraint but not as participants in the membership; schismatic sects that preserve the doctrine are systematically excluded from the institutional conversation; women in plural marriages have no authorized voice in determining their own legal status or spiritual standing; rank-and-file members who expected doctrine to be binding have no mechanism to contest the reversal.
% DISAPPEARANCE_RATIONALE: If the doctrine-practice gap disappeared (either by reinstating plural marriage publicly or by formally abandoning Section 132), the institution would lose the mechanism by which it maintains both doctrinal authority with fundamentalists and legal compliance with federal authorities. Institutional leadership would have to choose: become openly schismatic (reinstate practice, lose legal standing) or formally abjure the doctrine (lose theological claim with conservatives). The institutional survival structure collapses without the gap.
% FOUNDING_PROBLEM: A religious movement founded on continuous revelation and doctrinal restoration faced legal prohibition of a foundational practice (plural marriage). The founding problem: how to preserve doctrinal legitimacy while responding to external coercion without explicit doctrinal revision (which would appear as submission to secular authority).
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests that the founding problem persists and that the doctrine-practice gap is a legitimate solution (preserving doctrinal integrity while adapting practice). Federal authorities and schismatic groups attest that the founding problem reveals irreconcilable conflict between doctrine and law — they read the gap as evasion, not solution. Contemporary historians document that the institutional leadership used the gap strategically to maintain doctrinal authority over fundamentalists while appearing compliant to federal authorities; this corroboration comes from outside the institutional beneficiary seat and supports the contested (not live, not dead) status.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.68 to 0.82 as institutional leadership solidifies control over interpretive boundaries (sealed records, private theology, public denial). The gap itself is the extraction mechanism: members cannot contest the doctrine because it is officially preserved; members cannot rely on the doctrine because practice is suspended. Theater ratio rises from 0.42 to 0.68 because an increasing share of institutional activity is devoted to managing the contradiction itself — doctrinal statements reaffirming Section 132, administrative directives suspending practice, sealed-record ceremonies preserving the cosmology while public policy denies it. Suppression requirement is high (0.79 at interval end) because maintaining the gap requires active enforcement: censoring dissent from fundamentalists, managing member confusion, controlling records, ensuring bishops do not perform public ceremonies while privately solemnizing sealed marriages. The measurements are authored at shared time points so temporal alignment is unambiguous.
 *
 * PERSPECTIVAL GAP:
 *   Institutional leadership experiences this as adaptive coordination (preserving doctrinal truth while responding pragmatically to external constraint). General membership experiences it as broken promise (doctrine meant to be binding now appears optional). Fundamentalists experience it as institutional hypocrisy (doctrine preserved in secret, abandoned in public). Federal authorities experience it as evasion (the institution retained the practice in hidden form). The engine computes these divergences from the structural data: the leadership's directionality is beneficiary-end (controls interpretive boundary); members' directionality is target-end (told doctrine is binding, then told practice is suspended); fundamentalists are partially excluded (they would contest the gap, but institutional authority prevents their voice from reshaping doctrine). The claimed type (tangled_rope) captures the dual structure: genuine coordination problem (how to maintain theology under pressure) AND asymmetric extraction (leadership captures authority, members lose clarity).
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership is beneficiary: they gain interpretive flexibility and authority consolidation. General membership and fundamentalists are victims: they lose the clarity about what the institution commits them to. Affected women are powerless victims: their spousal status, property rights, and legal standing become contestable at leadership discretion. Federal authorities are observers: they applied pressure (external), but the constraint operates within the institutional field. Directionality for each stakeholder follows from their structural position: beneficiaries (d near 0.0) gain from the ambiguity; victims (d near 1.0) are extracted from by losing membership clarity; excluded/observer seats (d=0.5 or analytical) are not targets of the extraction but may be affected by spillovers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a tangled_rope, not a snare or piton, because (1) genuine coordination function exists (adapting theology to external pressure without explicit capitulation) and (2) asymmetric extraction is baked in (leadership gains interpretive authority, members lose it). A snare would be pure extraction with a fake coordination cover. A piton would have no beneficiary maintaining it and no clear victim experiencing it. Here, leadership clearly benefits (gains authority), members clearly lose (lose clarity), and the constraint requires active enforcement (managing the gap, controlling records, directing bishops). The coordination and extraction are not separable — they are one mechanism: using doctrinal preservation to buy membership compliance while using practice suspension to buy federal compliance. Mandatrophy is NOT present: the founding problem (maintaining doctrine under pressure) is still live, and the solution (doctrine-practice gap) is actively maintained by leadership. If the gap disappeared, institutional leadership would lose flexibility. The doctrine-practice gap is not a degraded remnant; it is an actively engineered constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_preservation_vs_sincere_abandonment,
    'Did institutional leadership preserve Section 132 doctrine as a genuine commitment (sincere belief that the doctrine is eternally true but contextually suspended) or as a strategic cover for doctrinal abandonment (belief that the doctrine is false or obsolete, but preservation is politically necessary)?',
    'Private correspondence and sealed institutional deliberations from the period; comparative analysis of doctrinal framing in sealed vs. public communications; long-term trajectory of Section 132 doctrine (if eventually formally abandoned, the preservation was likely strategic cover; if reaffirmed in subsequent eras, the preservation may have been sincere).',
    'If strategic cover, the extractiveness is higher (institutional leadership knowingly maintained a false doctrine for authority gains). If sincere, the extractiveness is lower (leadership genuinely believes the doctrine while suspending practice under duress). The distinction affects whether this is a snare (fake doctrine, real extraction) or a tangled_rope (real coordination problem, real but asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_preservation_vs_sincere_abandonment, empirical, 'Whether doctrinal preservation was sincere or strategic cover.').

omega_variable(
    member_agency_vs_identity_lock,
    'Could general members have meaningfully exited the identity-locked state (identity_locked exit option) by forming independent faith communities that reinterpret or abandon Section 132, or were the kinship, property, and social costs so high that identity-lock is irreversible in practice?',
    'Historical data on members who attempted exit (schism rates, defection patterns, cost barriers to forming alternative communities); post-exit outcomes for fundamentalist schismatics (did they maintain social cohesion, property rights, kinship ties, or did these collapse after institutional separation).',
    'If exit is reversible (members could leave and rebuild community), identity-lock is overstated and members have more agency than the classified constraint suggests — directionality would shift toward more mobile for general membership. If exit is effectively permanent (severing identity means losing kinship, property, and social standing), identity-lock is confirmed and extractiveness is higher because the gap cannot be challenged without catastrophic personal cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_agency_vs_identity_lock, empirical, 'Whether identity-locked membership is truly irreversible or whether exit pathways existed.').

omega_variable(
    sealed_record_doctrine_sincerity,
    'Were sealed ceremonies and private celestial-marriage doctrine (documented in closed records) performed with genuine belief in their spiritual/eternal reality, or were they administrative performances designed to maintain doctrinal consistency in records while the institution had de facto abandoned the practice?',
    'Historian analysis of sealed-record practices, prayers, and theology; comparative study of pre-reversal and post-reversal sealed ceremonies (if theology and practice shifted, sincerity is questionable); testimony from bishops and sealers about their understanding of the ceremonies'' meaning and binding force.',
    'If genuine, sealed marriage doctrine represents a real coordination mechanism (members can maintain spiritual belief in eternal marriage in private sphere while complying with public law). If performative, sealed ceremonies are theater designed to maintain the appearance of doctrinal consistency — theater_ratio would be even higher than 0.68, and the constraint would be more extractive (pure authority capture with a thin doctrinal facade).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sealed_record_doctrine_sincerity, empirical, 'Whether sealed ceremonies and private doctrine were sincere or theatrical performance.').

omega_variable(
    alternative_kernel_framings,
    'Is the kernel fundamentally about celestial marriage (Section 132 as written: eternal binding commitment) or about divine flexibility (the deeper principle that God can adapt doctrine to circumstances)? If the latter, the practice-doctrine-gap reading reinterprets the kernel itself rather than preserving it.',
    'Analysis of institutional theology pre-reversal and post-reversal: did institutional theology frame the founding principle as ''celestial marriage'' (concrete practice) or ''divine flexibility'' (abstract meta-principle that permits practice change)? If the framing shifted post-reversal to emphasize flexibility, the preservation claim is anachronistic.',
    'If the kernel is truly about celestial marriage (practice), the practice-doctrine-gap reading does preserve the kernel (doctrine intact, practice suspended). If the kernel is really about divine flexibility (principle), the practice-doctrine-gap reading actually instantiates the endogenous_reinterpretation reading (the kernel is reinterpreted to permit suspending practice). This omega dissolves the distinction between the sibling readings and suggests the actual kernel is ambiguous at t0.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_kernel_framings, conceptual, 'Whether the kernel is about a specific practice (celestial marriage) or an abstract principle (divine flexibility), which affects whether the reading preserves or reinterprets the kernel.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) structural — external coercion (federal law, institutional authority, record control) — or internalized — members'' own cognitive dissonance and shame about the gap that they police internally without external enforcement?',
    'Post-exit suppression trajectory: if members who left the institution reported persistent shame/confusion about the gap, suppression is partly internalized. If they recovered clarity once institutional authority was no longer present, suppression was primarily structural. Comparative study of members with high institutional integration vs. peripheral members: if peripheral members report lower suppression (less identity-locked, less invested in the doctrine), structural vs. internalized distinction is clarified.',
    'If suppression is primarily internalized, extractiveness is effectively higher — the institution no longer needs to actively suppress dissent because members have incorporated the contradiction as personal cognitive dissonance. This is the more potent form of extraction and suggests the constraint persists even outside institutional reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of the doctrine-practice gap contradiction is structurally enforced or internalized by members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t2, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 2, 0.51).
narrative_ontology:measurement_basis(marr_tr_t2, observed).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.58).
narrative_ontology:measurement_basis(marr_tr_t4, observed).
narrative_ontology:measurement(marr_tr_t7, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 7, 0.66).
narrative_ontology:measurement_basis(marr_tr_t7, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.68).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.68).
narrative_ontology:measurement_basis(marr_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t2, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 2, 0.73).
narrative_ontology:measurement_basis(marr_be_t2, observed).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.77).
narrative_ontology:measurement_basis(marr_be_t4, observed).
narrative_ontology:measurement(marr_be_t7, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 7, 0.81).
narrative_ontology:measurement_basis(marr_be_t7, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.82).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.82).
narrative_ontology:measurement_basis(marr_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t2, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 2, 0.69).
narrative_ontology:measurement_basis(marr_su_t2, observed).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.74).
narrative_ontology:measurement_basis(marr_su_t4, observed).
narrative_ontology:measurement(marr_su_t7, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 7, 0.78).
narrative_ontology:measurement_basis(marr_su_t7, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.79).
narrative_ontology:measurement_basis(marr_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__practice_doctrine_gap, 0.15).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage-commitment-reversal kernel. The kernel itself — the institution's commitment to celestial marriage as eternal and binding — is decomposed into three structurally distinct constraint stories reflecting three readings. (1) practice_doctrine_gap (this file): doctrine preserved, practice suspended, structural ambiguity persists. (2) endogenous_reinterpretation: doctrine reinterpreted via new revelation (Woodruff's vision) to permit the reversal; kernel is substantively changed. (3) exogenous_override: federal coercion forces practice reversal without internal doctrinal revision; kernel is formally intact but operationally overridden. All three stories share the same time period (1890-1904) and the same historical event (the reversal) but instantiate different narratives about what the reversal means for the institution's commitment to the doctrine. The ε-invariance principle requires separate stories because the three readings exhibit substantially different extractiveness: practice_doctrine_gap is high-extraction (ambiguity gives institutional leadership unprecedented interpretive authority); exogenous_override is medium-extraction (federal pressure is the primary mechanism, doctrine is secondary); endogenous_reinterpretation is lower-extraction (internal revelation legitimizes the change, no structural ambiguity required). The sibling stories are linked by network.affects_constraints so that the three readings are discoverable as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__practice_doctrine_gap, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
