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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant (Immutable Commandment Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   D&C 132 (Doctrine and Covenants section 132, received 1843, published
 *   1852) presents polygamy as an eternal covenant required for the highest
 *   degree of exaltation — restoration of the ancient patriarchal order and a
 *   doctrine as immutable as divine nature itself. The immutable-commandment
 *   reading treats this revelation as binding doctrine: the commandment
 *   cannot be revised, suspended, or reinterpreted by later authority without
 *   denying the original revelation's divine source. Under this reading, the
 *   1890 Manifesto (which suspended practice but claimed not to revoke
 *   doctrine) created an unresolved contradiction: the commandment remains
 *   eternally valid, yet the institutional church withdrew enforcement and
 *   excommunicates those who practice it. This constraint models the
 *   structural bind that reading creates: compliance with federal law means
 *   apostasy from the revealed doctrine; compliance with revealed doctrine
 *   means federal prosecution. Federal pressure and institutional authority
 *   convergence on the same target (practitioners of the immutable doctrine)
 *   create a martyrdom constraint with no legitimate exit. This is ONE
 *   READING of the kernel 'eternal_marriage_covenant'; sibling readings
 *   interpret the same revelation differently (prophetic_override holds that
 *   living revelation can supersede prior revelation; temporal_accommodation
 *   holds that the doctrine remains valid eternally while practice obedience
 *   is suspended).
 *
 * KEY AGENTS:
 *   - early_polygamist_leadership: male church authorities and founders (Joseph Smith, Brigham Young), primary beneficiaries of the doctrine through legitimate plural marriage, authority to interpret scripture
 *   - wives_without_legal_status: women in unrecognized plural marriages, victims of the constraint — legal invisibility, child custody vulnerability, social marginalization, identity fusion with exaltation narrative
 *   - women_entering_plural_marriage: women taught that exaltation requires accepting plural marriage, targets of extraction (lost legal status, emotional labor of managing jealousy/rivalry, subordinate position in family structure)
 *   - dissident_members_rejecting_doctrine: church members who reject the immutability reading (accepting temporal_accommodation or prophetic_override), caught between institutional pressure to renounce practice and doctrine-level loyalty
 *   - federal_authorities: external constraint agent (US government, prosecuting polygamy), applying coercive pressure that amplifies the internal contradiction
 *   - institutional_authority_post_1890: the church leadership after the Manifesto, administering the doctrine-practice split, excommunicating fundamentalists who hold the immutable-commandment reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.78).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.81).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9').
narrative_ontology:cs_kernel_codification('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', fixed_text).
narrative_ontology:cs_authority_grounding('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', lineage).
narrative_ontology:cs_interpretation_layer_present('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9').
narrative_ontology:cs_reading_relation('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', foundational, polygamy_eternally_binding_immutable).
narrative_ontology:cs_axiom_status(polygamy_eternally_binding_immutable, holdable).
narrative_ontology:cs_axiom_grounding('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', polygamy_eternally_binding_immutable, deontological).
narrative_ontology:cs_axiom('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', foundational, prophetic_authority_cannot_revise_eternal_covenant).
narrative_ontology:cs_axiom_status(prophetic_authority_cannot_revise_eternal_covenant, holdable).
narrative_ontology:cs_axiom_grounding('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', prophetic_authority_cannot_revise_eternal_covenant, deontological).
narrative_ontology:cs_axiom('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', secondary, exaltation_requires_covenant_acceptance).
narrative_ontology:cs_axiom_status(exaltation_requires_covenant_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', exaltation_requires_covenant_acceptance, deontological).
narrative_ontology:cs_reference_frame('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', eternal_patriarchal_covenant_immutable).
narrative_ontology:cs_drift_state('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', post_manifesto_institutional_suppression, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a4cc9207-f3ee-4fb5-b3d4-911455b1c7a9', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, early_polygamist_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, doctrine_authority_structure).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, wives_without_legal_status).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, women_entering_plural_marriage).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissident_members_rejecting_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, women_entering_plural_marriage).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissident_church_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_practitioners_post_1890).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_born_in_covenant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male church authorities (Joseph Smith, Brigham Young, and their successors) who received or transmitted the revelation and benefited from legitimate access to plural marriage. They interpreted and enforced the doctrine, organized the practice, and derived authority and reproductive advantage from the claim that the covenant was revealed by God. Their exit options are analytical because they author the constraint's interpretation; they have no reason to exit a system that legitimizes their power.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, early_polygamist_leadership, agenda_setter,
    powerful, generational, analytical, regional).

% Women in unrecognized plural marriages, bearing the cost of legal non-recognition (no inheritance rights, no custody claim on children, no divorce protection). They are materially dependent on the husband, socially marginalized if the marriage becomes public, and taught that exaltation requires accepting the arrangement. Their belief in the doctrine's truth is the suppression mechanism; exit means renouncing exaltation. They are identity-locked because their sense of worthiness and eternal destiny is fused with acceptance of the covenant.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, wives_without_legal_status, payer,
    powerless, civilizational, identity_locked, regional).

% Women taught that exaltation requires accepting plural marriage, who enter the covenant. They receive social status within the community (as selected by leadership for the covenant), spiritual prestige (chosen for exaltation), and family belonging. They also bear the cost: loss of legal status if the marriage is not recognized, emotional labor managing jealousy and rivalry with co-wives, subordinate position in family hierarchy (the husband has authority over all wives), and vulnerability if the husband dies or abandons the household. Their exit is constrained by geographic isolation, economic dependency, and social pressure from the community.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, women_entering_plural_marriage, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, women_entering_plural_marriage, beneficiary).

% Church members who question or reject the immutable-commandment reading — who adopt the temporal_accommodation or prophetic_override reading instead. They are caught between institutional pressure to accept the Manifesto's suspension of practice and the immutable-commandment reading that remains canonical doctrine. If they teach prophetic_override or reject the doctrine, they face institutional opposition; if they remain silent, they carry cognitive dissonance about the unresolved contradiction. Their exit from the church is constrained by family ties, social community, and spiritual identity.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissident_church_members, payer,
    moderate, biographical, constrained, regional).

% US government authorities prosecuting polygamy under the Morrill Anti-Bigamy Act and subsequent legislation. They investigate and prosecute polygamists, seize property, and incarcerate offenders. They are external to the constraint but create the coercive pressure that transforms it from internal doctrine enforcement to martyrdom constraint. They have no exit from enforcement (it is the law); their analytical position is that of the external constraint agent.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_authorities, observer,
    institutional, generational, analytical, national).

% Church leadership after the 1890 Manifesto, administering the doctrine-practice split. They maintain that the doctrine is eternally true while suspending practice. They excommunicate fundamentalists who continue to practice the covenant, enforcing institutional loyalty to the temporal_accommodation reading. They are constrained by the contradiction between doctrinal claim and practice suspension; they cannot revoke the original revelation without delegitimizing the entire authority structure, but they must appear compliant with federal law to avoid continued prosecution and enable institutional survival.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, institutional_authority_post_1890, agenda_setter,
    institutional, generational, constrained, global).

% Contemporary and historical communities that hold the immutable-commandment reading against the institutional church's temporal_accommodation position. They practice plural marriage under the belief that the doctrine is eternally binding and the Manifesto was institutional apostasy. They face federal prosecution (as did early practitioners), institutional excommunication, and social marginalization. They are identity-locked because their entire worldview is organized around the belief that the immutable-commandment reading is the true doctrine; exit would require renouncing that belief and accepting institutional authority.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_practitioners_post_1890, payer,
    powerless, generational, identity_locked, regional).

% Children born to plural marriages, many without legal recognition. They experience the constraint's effects: family instability if the arrangement ends, legal vulnerability (no clear inheritance claim, custody uncertainty), social stigma if the family's structure becomes public, and often trauma if the mother leaves and must abandon custody claims. They are taught that their legitimacy and exaltation depends on the plural marriage being valid; exit from the family or from the faith carries the belief of lost exaltation. They are identity-locked by socialization into the constraint.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_born_in_covenant, payer,
    powerless, civilizational, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, early_polygamist_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes reproduction, family hierarchy, and authority legitimacy within a patriarchal model; coordinates sexual access and child-rearing arrangements; vindicates male leadership's claim to received divine authority; establishes a celestial kinship structure claimed to be necessary for the highest exaltation.
% TRANSFER_FUNCTION: Moves women's legal status, reproductive autonomy, and social dignity from women to male leadership and the institution; transfers the authority to interpret and manage the covenant from women (who have no voice in its formulation) to men (who author and enforce it). Material transfers include household resources, child custody, and inheritance claims.
% ABSENT_VOICES: Women in plural marriages have no voice in the formulation of the doctrine, no seat at the table where its continuation is debated, and no authority to interpret it. Federal authorities (who prosecute the practice) are external observers. Fundamentalist practitioners are initially excluded from the post-Manifesto institutional church; dissidents who question the doctrine are marginalized. The constraint's persistence depends on silencing the women it governs most directly.
% DISAPPEARANCE_RATIONALE: In the institutional church frame (post-Manifesto), the constraint has already largely disappeared in practice; the world rearranged after 1890 to operate without the practice, though the doctrine persists. In the fundamentalist frame, if the immutable-commandment reading disappeared (replaced by temporal_accommodation or prophetic_override), the world would rearrange: believers would no longer feel obligated to practice the covenant, and the authority justification for fundamentalist separation from the mainstream church would collapse. For women and their children currently under the constraint, disappearance would rearrange their legal status, inheritance claims, and family relationships.
% FOUNDING_PROBLEM: Restore the patriarchal order of ancient Israel and enable the highest degree of exaltation (the celestial kingdom and eternal increase — spiritual progeny and godhood); organize reproduction and family under divine rather than civil law.
% FOUNDING_PROBLEM_CORROBORATION: The institutional church (post-Manifesto) claims the founding problem is solved through reinterpretation: obedience to civil law on marriage is now the divinely-sanctioned path to exaltation. Fundamentalist practitioners attest the founding problem remains unsolved — the commandment is unfulfilled and exaltation is unattainable without the covenant. Historians outside the believing communities document that the practice was constructed to consolidate male power, expand reproduction of a founder-aligned bloodline, and control women's sexual and economic autonomy. Federal prosecutors testified that the practice was designed to extract compliance through coercion. No external, non-interested party corroborates that the founding problem (exaltation via covenant) is the authentic historical origin; the founding problem narrative is attested only by beneficiaries or by believers within the tradition.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, contested).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   This constraint exhibits sustained high extraction (0.78 end-state) and suppression (0.81) because the immutability doctrine forecloses legitimate paths to exit: a woman cannot renounce plural marriage without renouncing the doctrine of exaltation; a male leader cannot acknowledge the doctrine's invalidity without denying his authority to have received revelation. The suppression is both structural (legal invisibility under federal law, family dependency, geographic isolation in Utah communities) and internalized (belief that refusing the covenant forecloses exaltation, identity fusion with the role of plural wife). The measurement series shows critical dynamics: (1) steady extractiveness through the 1840s-1880s as the constraint operates in its pure form; (2) sharp drop at the 1890 Manifesto (extractiveness 0.82→0.45) when the institutional church suspends practice but claims doctrine survives — the reading's core contradiction emerges here; (3) recovery to 0.78 in contemporary fundamentalism, where groups hold the immutable-commandment reading and extract from believers who accept it, now against the institutional church's prohibition. The theater_ratio spike at the Manifesto (0.38→0.72) indicates doctrine-practice splitting: enforcement machinery now maintains a claim that is not enforced, performative preservation rather than functional coordination. Post-Manifesto institutional theater is high (0.68) because the constraint persists as doctrine while practice is criminalized; suppression rises again (0.91 at Manifesto, settling to 0.81) because enforcement now operates against the doctrine's own practitioners. The constraint is tangled_rope (not snare) at the early phase: there is genuine coordination function (reproducing the authority structure, organizing reproduction and family, legitimizing male leadership); there is also asymmetric extraction (women bear the cost, leadership benefits from the claim to authority). Post-Manifesto, the constraint fragments: institutional church reads it as temporal_accommodation or prophetic_override, making it a different constraint; fundamentalist communities hold the immutable-commandment reading and operate it as a snare under institutional opposition.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (women in plural marriage) and the agenda-setter seat (early leadership) should compute radically different types. From the leadership seat, the constraint is rope — genuine coordination (organizing family, reproduction, authority legitimacy) with real coordination benefits that participants accept for exaltation. From the target seat (women), the same constraint operates as snare — the coordination narrative is cover for extraction (male authority, reproductive control, legal invisibility), and the suppression (identity lock to exaltation doctrine) is what holds it in place. The engine computes this divergence from the structural data: different power levels, different exit options, different beneficiary/victim status, different spatial scope (women are often geographically trapped in closed communities; leadership has continental/global option through migration). The post-Manifesto institutional authority introduces a third perspective: from their seat, the constraint should compute as piton (theater_ratio 0.68 at end-state, suppression high but extraction lower in the institutional frame than in the fundamentalist frame, because the institutional church no longer benefits from the practice — it benefits from renouncing the practice and claiming doctrinal consistency). The authored claim (tangled_rope) reflects the early phase when coordination and extraction were structurally fused; the metrics reflect the full interval including the Manifesto transformation.
 *
 * DIRECTIONALITY LOGIC:
 *   The immutable-commandment reading creates three distinct directionality clusters. (1) Early polygamist leadership: d ≈ 0.1 (full beneficiary cluster). They receive legitimate authority, sexual/reproductive access, expanded household wealth, and divine vindication. They are not trapped (powerful, exit options are analytical — they author the constraint). (2) Women in plural marriage: d ≈ 0.85–0.95 (full target cluster). They are identity-locked (the doctrine teaches exaltation requires acceptance; exit means apostasy from salvation). Their exit options collapse: geographic isolation in Utah, legal non-recognition, child custody vulnerability if they flee, social excommunication. Time horizon is civilizational (exaltation is eternal). Power is powerless (they have no authority to interpret doctrine or exempt themselves). (3) Post-Manifesto institutional authority: d ≈ 0.4–0.5 (moderate asymmetry). They maintain a doctrine they no longer enforce, creating cognitive dissonance for members. They face federal pressure and institutional pressure; their exit would be renouncing the original revelation (high cost to authority claim). Their suppression is performative rather than extractive (they excommunicate fundamentalists to appear compliant to federal authorities, not to collect from the constraint). Dissident members rejecting the doctrine: d ≈ 0.75 (targets of institutional suppression if they teach prophetic_override or temporal_accommodation as the true reading, but targets of fundamentalist communities if they remain in groups holding the immutable-commandment reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is explicit in D&C 132: restore the order of ancient patriarchs, organize the celestial family, enable the highest exaltation. The founding_problem_status is CONTESTED: the institutional church (post-Manifesto) treats the problem as solved by reinterpreting 'obedience' to mean obedience to civil law rather than to the covenant practice. Fundamentalist holders of this reading treat the problem as unsolved — the commandment remains unfulfilled. The disappearance_verdict is CONTESTED: if the immutable-commandment reading disappeared (replaced by prophetic_override or temporal_accommodation), the world would rearrange for fundamentalist communities (they would lose the justification for practice, or accept that revision is legitimate), but the institutional church has already rearranged itself to operate without it. The mismatch signals mandatrophy: the constraint's original problem (celestial exaltation via covenant obedience) is no longer the driver of its current form (institutional doctrine-practice split, fundamentalist resistance to institutional authority). The constraint now persists because (1) the institution cannot renounce the original revelation without delegitimizing itself, and (2) fundamentalist communities draw identity from rejection of the institutional church's temporal_accommodation reading. The founding problem is dead for the institutional church (they have moved past it), but the doctrine persists as zombie constraint maintained by the contradiction between institutional authority claim and institutional practice. For fundamentalist holders of this reading, the founding problem is live and urgent — their entire practice is organized around it. The constraint exhibits mandatrophy in the institutional form: it is theater (0.68 post-Manifesto) without function (doctrine is no longer enforced), maintained only because revocation would fracture the authority narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_practice_gap,
    'Does the immutable commandment reading''s assertion of eternal validity remain structurally coherent after the 1890 Manifesto suspended practice without renouncing doctrine?',
    'Textual/hermeneutical analysis: does the doctrine-practice distinction hold logically, or does practice suspension implicitly revoke the immutability claim? Examine post-Manifesto canonical statements from doctrine authority.',
    'If the distinction holds, the reading survives as a doctrine-level claim decoupled from enforcement. If it collapses, the immutable-commandment reading forecloses itself through internal contradiction, and either the temporal_accommodation or prophetic_override reading becomes the live framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_gap, conceptual, 'Whether immutability can survive permanent practice suspension without logical collapse.').

omega_variable(
    identity_lock_mechanism,
    'For women entering plural marriage under this reading, is suppression of exit primarily structural (legal barriers, family economics, geographic isolation) or internalized (identity fusion with the covenant, belief that apostasy forecloses eternal exaltation)?',
    'Post-exit trajectories: if women who leave carry suppression-related distress (shame, identity fracture, belief in lost exaltation), the suppression is at least partially internalized. If constraints ease after geographic/institutional exit, suppression is more structural.',
    'Internalized suppression means the constraint''s effective extractiveness is higher than the structural measurement suggests — the target carries the suppression after the external mechanism is removed. Would increase computed χ and shift classification likelihood toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Suppression mechanism: structural vs. internalized in identity-locked agents.').

omega_variable(
    divine_origin_vs_constructed_benefit,
    'Is the immutable-commandment claim genuinely a natural law (divine utterance, non-negotiable) or a constructed constraint whose immutability narrative benefits identifiable parties (male leadership, doctrine authority)?',
    'False summit analysis: the reading declares beneficiaries exist (early leadership, authority structure). A genuine mountain would have no beneficiary set. The immutability claim rides authority-grounding lineage; if that authority is shown to have motive (reproduction of male power, institutional centralization), immutability becomes suspect.',
    'If beneficiary motive is established, the reading reclassifies from mountain to tangled_rope/snare via FSM. If immutability is shown to rest on authority-extraction (institutional benefit from the claim), the axiom status shifts from holdable to overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_origin_vs_constructed_benefit, conceptual, 'Natural law vs. constructed constraint with institutional beneficiaries.').

omega_variable(
    federal_pressure_as_external_or_intrinsic,
    'Does federal anti-polygamy pressure (prosecutions, property seizure, loss of franchise) create the martyrdom constraint, or does the reading''s own immutability axiom create it by foreclosing prophetic revision?',
    'Counterfactual: would the constraint persist with the same extraction profile in the absence of federal pressure? Examine doctrine statements prior to and independent of persecution context.',
    'If federal pressure is the primary driver, the constraint is contingent on external coercion and would collapse if legal pressure lifted. If immutability doctrine itself creates the bind (by forbidding revision regardless of external circumstance), the constraint is self-perpetuating and would persist even without federal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_as_external_or_intrinsic, empirical, 'Whether the constraint''s persistence depends on external coercion or on internal doctrine logic.').

omega_variable(
    kernel_reading_identity_under_manifesto,
    'After the 1890 Manifesto, does the immutable-commandment reading survive as a live doctrine claim held by some members, or does it become a zombie reading held only by dissenters against the institutional authority''s revised reading?',
    'Institutional stance tracking: does the doctrine authority (post-1890 leadership) explicitly hold or renounce the immutable-commandment reading? Do institutional dissidents (Fundamentalists) hold it as the true doctrine against institutional corruption?',
    'If the authority renounces it, the reading becomes a schismatic/fundamentalist position (smaller, more isolated agent set). If the authority preserves it as doctrine-level truth while suspending practice, it remains canonical but unenforced. Position determines the stakeholder set, exit costs for dissenters, and the constraint''s effective suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_under_manifesto, empirical, 'Post-Manifesto institutional stance on the immutable-commandment reading''s validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(measurement_theater_1840s, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(measurement_theater_1850s, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(measurement_theater_1860s, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(measurement_theater_1870s, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(measurement_theater_1880s_pre_manifesto, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(measurement_theater_1890_manifesto_doctrine_practice_split, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 21, 0.72).
narrative_ontology:measurement(measurement_theater_1890s_post_manifesto_institutional, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement(measurement_theater_present_day_fundamentalism, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(measurement_extractiveness_1840s, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(measurement_extractiveness_1850s, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 5, 0.81).
narrative_ontology:measurement(measurement_extractiveness_1860s, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(measurement_extractiveness_1870s, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(measurement_extractiveness_1880s_pre_manifesto, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(measurement_extractiveness_1890_manifesto_drop, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 21, 0.45).
narrative_ontology:measurement(measurement_extractiveness_1890s_post_manifesto, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(measurement_extractiveness_present_day_fundamentalism, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(measurement_suppression_1840s, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(measurement_suppression_1850s, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(measurement_suppression_1860s, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(measurement_suppression_1870s, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(measurement_suppression_1880s_pre_manifesto, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(measurement_suppression_1890_manifesto_enforcement_intensification, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 21, 0.91).
narrative_ontology:measurement(measurement_suppression_1890s_post_manifesto, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(measurement_suppression_present_day_fundamentalism, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__immutable_commandment_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel admits three structurally distinct constraint readings: immutable_commandment_reading (this story), prophetic_override_reading, and temporal_accommodation_reading. Each reading declares a different ε (immutable is high extractiveness + immutable doctrine; prophetic_override is lower because revision is possible; temporal_accommodation is lower because doctrine-practice split reduces enforcement). The three stories form a constraint family linked by network.affects_constraints. This immutable-commandment reading forecloses the prophetic_override reading (if the covenant is eternally immutable, prophetic override is illegitimate) but coexists with the temporal_accommodation reading in institutional contexts (though unstably, generating mandatrophy). The family structure reflects the kernel contest: the same text (D&C 132) yields different constraints depending on the reading's axioms about revision authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
