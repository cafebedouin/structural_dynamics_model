% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: State Principled Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'principled intervention' reading of the
 *   constitutional secularism kernel—the claim that the state may
 *   legitimately intervene in religious affairs when necessary to advance
 *   social reform and protect weaker sections within religious communities.
 *   This reading authorizes state courts to judge whether religious practices
 *   cause 'harm' and to mandate reforms against community resistance,
 *   provided the intervention is justified as protective of vulnerable
 *   members. The reading differs from strict neutrality (state maintains
 *   equal distance) and from reformism (state has affirmative duty to
 *   eliminate oppression, subordinating religious autonomy entirely). The
 *   principled-intervention reading attempts to occupy a middle ground:
 *   intervention is permissible when reform-necessity is established, not
 *   prohibited (strict neutrality) but not unconditionally mandated
 *   (reformism). The constraint's extractiveness trajectory shows rising
 *   intensity over 30 years as intervention authority expanded and deepened,
 *   with plateau at high extraction (0.68) once judicial doctrine solidified;
 *   resistance (0.72) remains high throughout because community authorities
 *   contest the legitimacy of secular judgment over religious matters.
 *
 * KEY AGENTS:
 *   - State Judiciary: Interprets constitutional authority; sets intervention boundaries; decides 'harm' and 'reform necessity'
 *   - Reform Advocates (organized civil society): Petitions courts; identifies targeted practices; legitimizes intervention as protective
 *   - Marginalized Religious Minorities (women, lower castes, minorities-within-minorities): Vulnerable members seeking protection but absorbed into majoritarian court readings of their traditions
 *   - Traditional Religious Authorities: Custodians of religious law whose interpretive monopoly is displaced by secular review
 *   - Majoritarian Religious Coalitions: Numerically/politically dominant religions; less frequently targeted; selectively benefit from intervention against minorities
 *   - Religious Dissidents (excluded): Internal reformers whose perspectives are structurally unavailable to courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "State Principled Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '9d3a3132-70f6-4eae-8ab1-0a4c78fec2da').
narrative_ontology:cs_kernel_codification('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', formalized).
narrative_ontology:cs_authority_grounding('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', extraction).
narrative_ontology:cs_interpretation_layer_present('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da').
narrative_ontology:cs_reading_relation('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', constitutional_secularism__strict_neutrality_reading, influences).
narrative_ontology:cs_reading_relation('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', foundational, protective_intervention_legitimate).
narrative_ontology:cs_axiom_status(protective_intervention_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', protective_intervention_legitimate, deontological).
narrative_ontology:cs_axiom('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', secondary, state_interpretive_authority_bounded).
narrative_ontology:cs_axiom_status(state_interpretive_authority_bounded, holdable).
narrative_ontology:cs_axiom_grounding('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', state_interpretive_authority_bounded, conventional).
narrative_ontology:cs_reference_frame('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', protective_state_paternalism).
narrative_ontology:cs_drift_state('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', contemporary_majoritarian_capture_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9d3a3132-70f6-4eae-8ab1-0a4c78fec2da', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_advocates).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, majoritarian_religious_coalitions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, majoritarian_religious_coalitions).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, secular_state_doctrine).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, protective_paternalism_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional authority to intervene in religious affairs when deemed necessary for social reform. Sets the boundaries of acceptable intervention, decides which practices are oppressive, and enforces remedies. Justifies intervention as protecting vulnerable members within religious communities from harm. Sits outside the religious communities it regulates, claims expert standing on constitutional limits.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Civil-rights organizations, feminist groups, secular NGOs, and activist networks that seek state intervention to eliminate practices they identify as oppressive: widow immolation, child marriage, caste-based discrimination within temples, gender-segregated worship, religiously sanctioned domestic abuse. They frame intervention as protection of weaker sections and rely on state authority to override religious community resistance. Their power lies in advocacy capacity and ability to petition courts.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_advocates, beneficiary,
    organized, generational, mobile, national).

% Members of religious communities—women, lower castes, religious minorities within minorities—whose interests diverge from community leadership. They may seek state intervention to escape practices they experience as coercive but lack standing to challenge internally. However, intervention also subjects them to majoritarian judicial interpretation of their own traditions and may undermine community institutions they depend on for identity and material support.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities, payer).

% Clergy, temple administrators, monastery heads, community elders whose authority derives from custodianship of religious law and tradition. State intervention erodes their interpretive monopoly, subjects their decisions to secular judicial review, and reframes practices they defend as sacred into objects of state reform. They experience the constraint as coercive displacement of legitimate religious governance by an outside power.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditional_religious_authorities, payer,
    powerful, generational, constrained, regional).

% Larger, politically dominant religious communities (Hinduism in India, Islam in Muslim-majority states) whose practices are less frequently targeted for intervention because their numerical and political power influences court composition and public opinion. They face the threat of intervention but also benefit when courts interpret intervention as legitimizing their traditions against minority religions or when intervention targets minorities. Bear suppression costs selectively.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, majoritarian_religious_coalitions, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, majoritarian_religious_coalitions, beneficiary).

% Enacts statutory constraints on religious practice (personal law reforms, anti-dowry statutes, scheduled-caste protections) and delegates boundary-drawing to courts. Sits at a remove from judicial intervention but shapes its scope through legislative definition of 'harm' and 'reform.' Can alter the constraint through statute but faces political costs from religious constituencies.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, state_legislature, observer,
    institutional, generational, analytical, national).

% Internal reformers within religious communities (modernist clergy, feminist theologians, heterodox practitioners) who share reform goals but are excluded from the state-intervener seat. Their perspectives could complicate the binary of 'state' vs. 'tradition,' but the intervention framework structures them out as either co-conspirators with state overreach or insufficiently committed to their own tradition's liberation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_dissidents, excluded,
    moderate, biographical, constrained, local).

% UN bodies, treaty monitoring committees, and transnational NGOs that apply universal human-rights standards to religious practices, often supporting state intervention against 'traditional oppression.' Frame intervention as obligation under international law and measure state compliance. Sit outside domestic religious-political contestation but influence legitimacy narratives.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, state_judiciary).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a secular authority—the state judiciary—as the arbiter of permissible religious practice when those practices harm or restrict members of religious communities. Solves the coordination problem of how to protect vulnerable individuals (women, lower castes, minorities) who cannot exit religious communities but whose interests are subordinated by community leadership. Creates a single authoritative voice on 'acceptable' religious conduct, reducing the need for individuals to negotiate with multiple authorities (internal and external).
% TRANSFER_FUNCTION: Transfers interpretive authority over religious law and practice from decentralized community religious authorities to centralized state courts. Moves the power to define 'oppression,' 'harm,' and 'reform necessity' from religious communities to secular legal systems. Allocates enforcement resources (police, legal machinery, coercive power) to implement court-defined reforms against community resistance. The transfer is asymmetric: beneficiaries (reform advocates, marginalized members) receive judicial validation and coercive backing; payers (traditional authorities, reluctant community members) absorb erosion of autonomy and reputational harm.
% ABSENT_VOICES: Religious dissidents and reformers within communities—voices advocating change from inside their own traditions—are systematically excluded because the constraint's structure positions them as either state co-conspirators (if they cooperate with judicial intervention) or as failures of their own tradition (if they challenge state authority). Strict-neutrality interpreters and those who believe reform must come from within communities by consent are structurally kept out of the intervention seat. Conservative religious minorities whose reform objectives differ from majoritarian courts' priorities are also excluded from effective voice.
% DISAPPEARANCE_RATIONALE: If the principled-intervention authority vanished, vulnerable individuals within religious communities would lose access to state-backed remedies and would face a choice between enduring practices they experience as oppressive, attempting internal change with no institutional support, or exiting their communities entirely (a high cost). Religious communities would recentralize authority, and reform pressure would have to work through internal persuasion or exit rather than coercive law. The political economy of religious governance would shift back toward community autonomy, though power dynamics within communities might ossify without external accountability pressure.
% FOUNDING_PROBLEM: Religious communities contain practices that cause measurable harm to their members—particularly women, lower castes, and minorities—and community authorities resist internal reform. Individuals trapped in these communities cannot exit without losing livelihood, family, social identity, and material security. The founding problem is the gap between vulnerable individuals' interests and community authorities' resistance to change, combined with the state's obligation to protect all citizens.
% FOUNDING_PROBLEM_CORROBORATION: Reform advocates and human-rights organizations attest to documented harms: widows in extreme poverty, child brides, caste violence within temples, gender-segregated worship, religiously sanctioned domestic abuse. Some marginalized community members attest to these harms within their own experience. However, traditional religious authorities and conservative legal scholars attest that the founding problem has been substantially overstated, that harms are being reframed as 'traditional practice,' and that state intervention has become a tool of majoritarian religious politics rather than genuine protection. Competing empirical literatures show both documented harms and cases where intervention has destabilized protective community structures or has been weaponized against minorities.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint transfers interpretive authority from community religious figures to state courts and does so without requiring community consent or internal reform consensus. The justification is protective, but the mechanism is asymmetric transfer of power. Suppression is higher still (0.71) because the constraint requires active, ongoing enforcement against community resistance—courts must sustain intervention against religious authorities' non-compliance, social pressure, and political mobilization. Theater is moderate (0.42): the protective rationale is genuine (courts do prevent documented harms), but growing proportion of enforcement labor goes to defending the state's authority to intervene (doctrinal self-justification) rather than to the concrete remedies. The measurement series show extractiveness and suppression rising steeply for 20 years (judicial authority consolidating), then plateauing as doctrine stabilizes and community compliance patterns settle into low-intensity resistance. The time grid is shared across all three metrics; every metric is authored at every time point to ensure alignment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (religious authorities, majoritarian coalitions) and the beneficiary seats (reform advocates, state judiciary) compute fundamentally different constraint types from the same structural data. From the beneficiary perspective: this is genuinely protective coordination—the state creates a last-resort mechanism for those trapped in oppressive structures. From the payer perspective: this is extractive displacement—the state weaponizes 'protection' narratives to centralize interpretive authority over religious life and to remake traditions in majoritarian (or secular-elite) image. The marginalized-minority seat is the hardest to classify because it is simultaneously beneficiary and victim: protected from some practices, but subject to externally-imposed interpretations of its traditions and stripped of internal remedial authority. The engine computes per-seat types from the structural data; the divergence is not error but the core finding—seats with opposed directionalities and power asymmetries perceive the same constraint as having opposite functions.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judiciary sits near the beneficiary end (d ≈ 0.10–0.25): it exercises authority, sets rules, and is insulated from the communities it regulates; it faces minimal exit cost (its authority is constitutionally anchored). Reform advocates sit near the beneficiary end (d ≈ 0.15–0.35): they access state machinery to override community resistance and rely on coercive backing; their exit from the constraint is easy (they can withdraw from advocacy). Marginalized community members sit near the target end (d ≈ 0.60–0.75): they are the ostensible beneficiaries of protective intervention, but they are also subject to majoritarian judicial interpretation of their traditions and lose access to community-internal remedies; their exit is constrained (abandoning community structures means losing material support and identity). Traditional authorities sit near the full-target end (d ≈ 0.80–0.95): they pay the extraction (loss of interpretive authority, reputational damage, costly compliance), face high suppression (courts override their decisions), and have constrained exit (they cannot withdraw from the communities they lead). The divergence between the payer and beneficiary seats is stark: from the judiciary and reform-advocate seats the constraint appears as protective coordination; from the traditional-authority seat it appears as displacement and domination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (vulnerable individuals trapped in oppressive religious structures) remains contested and may be partially dead (many harms have been reduced through prior interventions and social change), but the constraint persists and deepens (extractiveness rising for 30 years). This is exactly the mandatrophy signal: an intervention justified by a founding problem whose status is contested is sustained by the constraint's own operation and by organized beneficiary pressure, not by evidence that the problem is live. The remedy for mandatrophy would require either: (1) establishing unequivocally that the founding problem is live and that community authorities continue to resist reform (resolution through demonstration), or (2) accepting that the founding problem is substantially dead and that the constraint now operates primarily to enforce majoritarian interpretive preferences over religious communities (resolution through acknowledgment). The constraint cannot simultaneously claim 'founding problem is live and intervention is necessary' and 'we have successfully reformed these practices.' If the latter is true, intervention should narrow; if the former is true, resistance should drop. Instead, resistance plateaus high and extraction stabilizes, characteristic of a constraint whose legitimating justification has become decoupled from its actual operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_vs_difference_ambiguity,
    'What distinguishes a religious practice that causes ''harm'' (justifying intervention) from one that simply violates secular liberal values but is voluntarily maintained by community members?',
    'Establish objective criteria for harm (measurable injury, lack of internal exit, coercion) and apply them consistently across religious traditions and classes of practice. Test whether harm determination tracks actual community-member reports or predominantly reflects secular-elite preferences. Compare harm prevalence before and after intervention to assess whether intervention reduced the reported harm or mainly changed legal status.',
    'If ''harm'' is construed broadly to include practices that conflict with secular values (e.g., gender roles, modesty norms, dietary rules), the constraint becomes an instrument for imposing secular culture on religious communities, moving it toward snare classification. If ''harm'' is narrowly defined (tangible bodily injury, non-consensual participation, trapped members), the protective rationale holds and the constraint remains tangled rope (genuine protection plus asymmetric authority transfer). The omega is critical because secular judges have systematic incentive to read traditions they do not understand as harmful.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_vs_difference_ambiguity, conceptual, 'Whether intervention targets measurable harm to individuals or reflects majoritarian aesthetic disapproval of religious difference.').

omega_variable(
    internal_vs_imposed_reform_split,
    'To what extent do the reforms imposed by state intervention align with reforms that reformers within the religious community were already pursuing, versus imposing reforms from outside against internal dissent?',
    'Compare the timing and substance of state-ordered reforms with documented internal reform movements in the same communities. Measure community-member acceptance (vs. coerced compliance) of reforms. Track whether marginalized members'' own reform priorities align with state intervention priorities or diverge. Interview reformed religious authorities to assess whether they view changes as legitimate internal evolution or as external imposition.',
    'If state intervention accelerates internally-driven reform, the constraint functions as empowerment (tangled rope with lower victim-cost asymmetry). If state intervention imposes reforms against internal consensus or co-opts minority internal movements to legitimate majoritarian agendas, the constraint becomes more extractive and moves toward snare. This omega addresses the risk that state intervention becomes a vehicle for majoritarian religious or secular groups to impose their vision on minorities by claiming ''protection of the vulnerable.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_imposed_reform_split, empirical, 'Whether state reforms align with and accelerate internal community reform or impose external preferences.').

omega_variable(
    majoritarian_capture_risk,
    'Do intervention decisions disproportionately target minority religions or lower-caste religious practices while treating majority-religion analogous practices as culturally acceptable?',
    'Conduct systematic audit of court decisions: identify analogous practices in majority and minority traditions, compare intervention frequency and remedy severity. Track whether court composition (majority religion, caste) correlates with intervention targets. Measure time lag between identification of practice and court action for majority vs. minority religions. Survey community members'' perceptions of whether intervention protects or targets their tradition.',
    'Evidence of systematic disparities (higher intervention rate against minorities, gentler treatment of majority practices) would indicate that the ''principled intervention'' framework has been captured by majoritarian religious politics, moving the constraint toward snare for minority communities. This would mean the protection function is real for some (marginalized members of majority religions) but the constraint simultaneously operates as majoritarian imposition on minorities. The constraint would remain tangled rope from the beneficiary seat but shift toward snare from the targeted-minority seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Degree of majoritarian capture of intervention authority and differential targeting of minorities.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Can the principled-intervention reading and the strict-neutrality reading coexist within a single constitutional framework, or does accepting one logically foreclose the other?',
    'Examine constitutional jurisprudence to determine whether courts hold both readings live (applying different tests in different contexts) or whether one reading has logically displaced the other (courts consistently choose one framework). Assess whether a constitution could coherently hold: ''the state maintains distance from religion in general (neutrality) but may intervene when specific protective grounds are met (principled intervention).'' Test whether this joint framework produces inconsistent verdicts about the same practices.',
    'If the readings coexist, the kernel supports both as live political positions and the constraint is contestable. If principled intervention logically forecloses strict neutrality (i.e., admitting intervention authority defeats neutrality), then no constitution can hold both and one reading has foreclosed the other—the constraint would appear as a resolution of a logical contradiction, not a choice among contestable positions. The finding would shift the reading_relations from ''coexists_with'' to ''forecloses'' and change how terminal stability is assessed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Logical compatibility of intervention-based and neutrality-based readings of constitutional secularism.').

omega_variable(
    identity_lock_religious_exit,
    'For marginalized community members seeking to exit oppressive religious structures via state intervention, is the identity-lock exit option genuine, or does state intervention substitute one form of identity capture for another (from religious to secular authority)?',
    'Track individuals who used state remedies to exit religious authority relationships; measure their post-intervention attachment to secular legal identity, their sense of agency, and their ability to maintain community ties or cultural affiliation without community authority mediation. Distinguish between individuals who exit the community entirely (secular adoption) versus those who reform the community from inside (leveraging state backing for internal change) versus those who remain trapped in secular legal identity without genuine mobility.',
    'If identity-lock persists post-intervention (individuals still cannot exit the identity frame, now located in secular legal status), then the constraint has transferred rather than removed identity-lock, making exit_options remain ''identity_locked'' or ''constrained'' rather than genuinely opening to ''mobile'' or ''arbitrage.'' This would increase the effective extraction on these stakeholders and move the constraint classification toward snare at their seat. If intervention genuinely opens exit (individuals can adopt new identity frames, secular or reformed-religious), the constraint functions as promised and extraction is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_religious_exit, empirical, 'Whether state intervention removes identity-lock or transfers it from religious to secular authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__principled_intervention_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__principled_intervention_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__principled_intervention_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__principled_intervention_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__principled_intervention_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__principled_intervention_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__principled_intervention_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__principled_intervention_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__principled_intervention_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__principled_intervention_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__principled_intervention_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__principled_intervention_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__principled_intervention_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__principled_intervention_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__principled_intervention_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__principled_intervention_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, personal_law_autonomy__religious_community_governance).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, caste_discrimination__temple_access_claims).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, gender_justice__religious_practice_reform).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member kernel family decomposed from 'constitutional secularism' under the ε-invariance principle. The kernel is the contested commitment that the state has some legitimate relationship to religion; the three readings advance structurally distinct claims about the nature and scope of that relationship. Strict neutrality and principled intervention have different ε values because they authorize fundamentally different state actions and create different beneficiary/victim structures. Principled intervention (this story) is substantially extractive from religious authorities (ε=0.68) because it transfers interpretive authority; strict neutrality is less extractive (null state action → lower extraction). All three readings are live in contemporary constitutional jurisprudence across different jurisdictions and judicial coalitions; no reading forecloses the others within the family. The family is linked bidirectionally: if principled intervention becomes doctrinally dominant, it influences the viability of strict neutrality by creating path-dependence in judicial authority (hard to recentralize once devolved). The readings occupy different institutional seats and represent different political coalitions; they coexist precisely because the kernel itself is contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
