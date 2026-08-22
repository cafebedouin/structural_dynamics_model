% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law as Instrument of Democratic Enclosure
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The Hong Kong National Security Law was imposed by China's National
 *   People's Congress Standing Committee in June 2020 without deliberation by
 *   Hong Kong's own legislature, in direct response to the 2019
 *   anti-extradition-bill protest movement. Read through the lens of what the
 *   law's actual multi-year operation has extracted from Hong Kong's
 *   political life, the law functions as the mechanism by which an entire
 *   tier of democratic infrastructure — competitive elections, an adversarial
 *   press, independent civil society, and lawful public dissent — has been
 *   dismantled and criminalized, with definitional vagueness (secession,
 *   subversion, collusion) doing the work of chilling activity well beyond
 *   anything resembling security threat.
 *
 * KEY AGENTS:
 *   - beijing_central_government: agenda_setter/beneficiary (institutional/analytical) — drafted and imposed the law, retains interpretive authority, bears no cost
 *   - hong_kong_establishment_bloc: beneficiary (powerful/mobile) — inherits uncontested political space
 *   - national_security_police_apparatus: agenda_setter/beneficiary (institutional/analytical) — administers daily enforcement with expanding scope
 *   - pro_democracy_opposition_politicians: payer (powerless/trapped) — mass arrest, disqualification, exile
 *   - independent_press_and_journalists: payer (moderate/constrained) — outlet closures, self-censorship
 *   - civil_society_organizations: payer (powerless/trapped) — forced self-dissolution
 *   - protest_participants_and_activists: payer (powerless/trapped) — retroactive prosecution of speech and symbols
 *   - general_hong_kong_public: payer/beneficiary (powerless/constrained) — trades political voice for claimed stability
 *   - foreign_governments_and_diaspora_advocates: excluded (powerful/analytical) — structurally barred from standing, engagement criminalized as collusion
 *   - hong_kong_judiciary: observer/payer (institutional/constrained) — formal independence intact elsewhere, discretion in national security cases narrowed near zero
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law as Instrument of Democratic Enclosure").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'd3997481-4371-42f6-929d-06594d989a1d').
narrative_ontology:cs_kernel_codification('d3997481-4371-42f6-929d-06594d989a1d', formalized).
narrative_ontology:cs_authority_grounding('d3997481-4371-42f6-929d-06594d989a1d', extraction).
narrative_ontology:cs_interpretation_layer_present('d3997481-4371-42f6-929d-06594d989a1d').
narrative_ontology:cs_reading_relation('d3997481-4371-42f6-929d-06594d989a1d', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('d3997481-4371-42f6-929d-06594d989a1d', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('d3997481-4371-42f6-929d-06594d989a1d', foundational, political_dissent_is_protected_expression_not_security_threat).
narrative_ontology:cs_axiom_status(political_dissent_is_protected_expression_not_security_threat, holdable).
narrative_ontology:cs_axiom_grounding('d3997481-4371-42f6-929d-06594d989a1d', political_dissent_is_protected_expression_not_security_threat, deontological).
narrative_ontology:cs_axiom('d3997481-4371-42f6-929d-06594d989a1d', foundational, enclosure_of_contestation_channels_constitutes_illegitimate_extraction_regardless_of_stated_security_rationale).
narrative_ontology:cs_axiom_status(enclosure_of_contestation_channels_constitutes_illegitimate_extraction_regardless_of_stated_security_rationale, holdable).
narrative_ontology:cs_axiom_grounding('d3997481-4371-42f6-929d-06594d989a1d', enclosure_of_contestation_channels_constitutes_illegitimate_extraction_regardless_of_stated_security_rationale, empirically_contingent).
narrative_ontology:cs_reference_frame('d3997481-4371-42f6-929d-06594d989a1d', one_country_two_systems_autonomy_baseline).
narrative_ontology:cs_drift_state('d3997481-4371-42f6-929d-06594d989a1d', post_2020_enactment_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d3997481-4371-42f6-929d-06594d989a1d', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_bloc).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press_and_journalists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, protest_participants_and_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_public).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law directly via the National People's Congress Standing Committee, bypassing Hong Kong's own legislature entirely. Defines the four core offenses (secession, subversion, terrorism, collusion with foreign forces) in terms broad enough to capture ordinary political speech and organizing. Retains the power to interpret the law's meaning and to claim jurisdiction over cases it designates as complex or involving foreign intervention. Bears essentially no cost from the law's operation and gains a permanently pacified periphery.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, beneficiary).

% Pro-Beijing legislators, business elites, and pro-establishment media benefit from the removal of electoral competition, the disqualification of opposition candidates, and the elimination of street-level political pressure that previously constrained governance. They face no criminal exposure under the law's practical enforcement and gain uncontested control of legislative and administrative processes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment_bloc, beneficiary,
    powerful, generational, mobile, regional).

% A newly created National Security Department with expanded investigatory powers — warrantless surveillance in specified circumstances, asset freezing, denial of bail as a structural default, and closed-door trials without jury for designated cases. Administers the law's daily operation, decides who is investigated, and has no institutional incentive to narrow its own scope.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, national_security_police_apparatus, beneficiary).

% Elected legislators and primary-election organizers have been mass-arrested, denied bail for years pending trial, disqualified from office, or forced into exile. Remaining in Hong Kong means indefinite pretrial detention risk for conduct — organizing primaries, publishing platforms — that was lawful political activity before the law's enactment. Exile means permanent severance from constituents and, for many, inability to ever return.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_opposition_politicians, payer,
    powerless, biographical, trapped, regional).

% Newsrooms such as Apple Daily and Stand News were raided, had assets frozen under the law, and were forced to shut down; editors and executives were charged with collusion for editorials and interviews. Surviving outlets self-censor extensively because the line between legitimate reporting and prosecutable 'collusion with foreign forces' is undefined and enforced retroactively. Exit means abandoning the local audience and often the jurisdiction itself.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press_and_journalists, payer,
    moderate, biographical, constrained, regional).

% Unions, professional associations, and advocacy groups — including the body that organized the annual Tiananmen vigil — have dissolved themselves under direct or implied threat of prosecution for their historical foreign contacts or advocacy positions, which now retroactively read as potential subversion or collusion. Continuing to operate risks asset freezing and leadership arrest; dissolving forfeits decades of accumulated civic infrastructure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    powerless, biographical, trapped, regional).

% Individuals who participated in or publicly supported the 2019 protest movement face prosecution for acts and speech that predate the law, applied with an effectively retroactive character through 'continuing offense' theories. Slogans, chants, and even displaying protest-era objects have supported convictions. Many face a choice between silence, emigration, or prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, protest_participants_and_activists, payer,
    powerless, biographical, trapped, local).

% Ordinary residents retain physical safety and commercial stability, which the law's proponents credit to it, but have lost the electoral, press, and associational channels through which they previously registered grievances or sought change in policy. Emigration has been the primary exit exercised by those with the means; the poorer and older majority experience the closure as their permanent political environment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_public, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, general_hong_kong_public, beneficiary).

% Governments imposing sanctions, offering asylum pathways, or issuing condemnations, and diaspora activists organizing abroad, have no standing within the law's own framework — indeed, engaging with them is itself criminalized as 'collusion with foreign forces' for anyone still in Hong Kong, which severs the constituency the law affects from the constituency positioned to object on its behalf from outside.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, foreign_governments_and_diaspora_advocates, excluded,
    powerful, generational, analytical, global).

% Common-law judges apply the law but do not control its content; national security cases route to a vetted panel of designated judges, and the Chief Executive holds power to certify evidence as a state secret, removing it from ordinary judicial scrutiny. Judges retain formal independence in unrelated matters but have watched their institution's discretion over the law's central cases narrow to near zero.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_judiciary, observer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hong_kong_judiciary, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Read charitably, the law claims to coordinate a jurisdiction's internal security apparatus with the sovereign state's, closing gaps that allowed a 2019-scale mass mobilization to threaten central authority — a genuine coordination problem exists between a sub-national jurisdiction's security posture and the sovereign's.
% TRANSFER_FUNCTION: Moves political voice, electoral contestability, press freedom, and associational capacity from Hong Kong's civil society, opposition, and independent media to Beijing's central government, the local establishment bloc, and the national security enforcement apparatus — a one-way transfer of institutional power with no reciprocal flow.
% ABSENT_VOICES: The pro-democracy politicians now imprisoned or exiled, the journalists whose outlets were shut down, and the diaspora advocacy networks are precisely the parties whose objections the law's drafting process excluded — the law was enacted without Legislative Council debate or Hong Kong public consultation, and post-hoc objection from abroad is itself criminalized as foreign collusion, closing the loop against corrective feedback.
% DISAPPEARANCE_RATIONALE: If the law were repealed overnight, opposition parties would reconstitute, dissolved civil society organizations would reform, independent media outlets would resume operation or new ones would launch, detained politicians and activists would be released, and Hong Kong's electoral and associational infrastructure — much of which existed continuously from the colonial and early handover periods until 2020 — would substantially re-emerge, indicating the arrangement is a constructed political enclosure rather than a background condition.
% FOUNDING_PROBLEM: The stated founding problem is the 2019 anti-extradition-bill protests escalating into sustained, sometimes violent, mass unrest that the Hong Kong government could not contain and that Beijing read as an existential threat to sovereign control and as inviting foreign interference.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong establishment attest the security threat remains live, citing ongoing overseas activism and foreign sanctions as continuing evidence of the same threat. Independent corroboration from outside the beneficiary set — UN human rights bodies, foreign bar associations, and academic legal scholarship on the law's post-2020 application — attests that the 2019-style mass mobilization capacity was already effectively ended by 2020 policing and pandemic-era assembly restrictions, and that the law's continued and expanding use against isolated individual speech acts, historical vigil organizers, and lawful primary elections indicates the founding security problem is substantially resolved while the arrangement persists and has broadened.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) and rising over the measured interval because the law's definitional breadth (secession, subversion, terrorism, collusion) has been used progressively to reach conduct further and further from anything resembling an organized security threat — historical vigil organizing, primary election coordination, individual protest slogans — while the beneficiary set's control has only consolidated. Suppression is authored even higher (0.92) than extraction because the law's persistence depends on continuously operating machinery: warrantless investigatory powers, bail-denial defaults, closed-door national-security-designated courts, and asset freezing, none of which are incidental to the law but constitutive of how it holds. Theater ratio is moderate (0.4) — some genuine security-relevant enforcement exists (foreign intelligence concerns are not wholly fictional) but a rising share of enforcement activity targets symbolic and expressive conduct with no plausible security nexus, which the rising trajectory captures. Accessibility collapse is authored high (0.85) because, from within Hong Kong, essentially no lawful channel for organized political opposition survives the law's chilling scope. Resistance is authored moderate (0.55) rather than low, reflecting continued underground organizing, diaspora activism, and periodic individual acts of defiance despite severe cost, which distinguishes this from a fully pacified population.
 *
 * PERSPECTIVAL GAP:
 *   From Beijing's and the establishment's seats, the law is a coordination success: order restored, institutions stabilized, foreign interference channels closed. From the payer seats, the identical legal text and enforcement apparatus is experienced as the permanent foreclosure of any lawful path to political change. The engine computing divergent per-seat classifications from these structural facts is precisely the point of authoring both sets of stakeholders honestly rather than resolving the tension in the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing sits at the far beneficiary end: institutional power, analytical exit (it is not itself governed by the law it authored), and civilizational time horizon — it collects a permanently pacified periphery at essentially zero direct cost. The Hong Kong establishment bloc similarly benefits, though with more exposure to reputational and economic costs from international response, hence 'mobile' rather than fully insulated exit. The enforcement apparatus is structurally a beneficiary of its own expanding mandate even though it is nominally an administrative body rather than a rent-collector. Opposition politicians, journalists, civil society, and protest participants sit at the extraction end: trapped or constrained exit, powerless relative to the apparatus, and bearing costs (imprisonment, exile, asset loss, career destruction) with no offsetting benefit. The general public occupies a genuinely mixed position — real stability benefit is not fictional — which is why it carries both beneficiary and payer roles, distinguishing this reading's honesty from a reading that would flatten the public into pure victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status/disappearance_verdict triangulation is where this reading's core claim is made falsifiable rather than asserted: if the founding security problem (2019-scale mass mobilization capacity) were genuinely still live, we would expect continued use against organized, coordinated, security-relevant conduct; what the corroborated record instead shows (independent legal scholarship, UN bodies) is expanding use against isolated individual expression, dissolved historical civic bodies, and lawful electoral primaries years after the mobilization capacity was already contained by other means. That status=contested + disappearance_verdict=world_rearranges pairing is the flag this framework is built to surface: a founding justification whose corroboration outside the beneficiary set has diverged from the beneficiary set's own account, while the arrangement has not narrowed but broadened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_nsl,
    'Is the NSL structurally a democratic-enclosure mechanism (this reading), a jurisdictional-capture mechanism eroding common-law autonomy (sibling reading), or a legitimate sovereignty-restoration instrument (sibling reading) — and is the disagreement resolvable by evidence or only by prior commitments about the legitimacy of the 2019 protest movement and of Beijing''s sovereign authority over Hong Kong?',
    'The three readings are not adjudicated within this story; each is authored as its own ε-invariant constraint (nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading) linked via network.affects_constraints. Resolution, if any, would come from longitudinal tracking of whether enforcement narrows toward genuine security threats (supporting sovereignty_restoration) or continues broadening toward ordinary political and expressive conduct (supporting democratic_enclosure), assessed by parties outside the beneficiary set.',
    'If future enforcement narrows sharply and independent observers corroborate a live, bounded security threat, this reading''s extraction claim would weaken and the sovereignty_restoration reading would gain support. If enforcement continues broadening as observed 2020-2024, this reading''s classification (snare, high extraction, high suppression) is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_nsl, conceptual, 'Which of three structurally distinct readings of the NSL kernel is descriptively dominant, and whether that is an empirical or a values question.').

omega_variable(
    beneficiary_boundary_general_public,
    'Does the general Hong Kong public''s claimed stability benefit reflect a genuine coordination gain (order and predictability that most residents value), or is the ''stability'' itself partly constructed by the same suppression that eliminated the comparison case (a Hong Kong with contained but still-functioning democratic contestation)?',
    'Comparative analysis against jurisdictions that addressed 2019-type unrest through negotiated political reform rather than criminalization, and survey/attitudinal research conducted with methodological independence from state security services (a serious data-access constraint under the law itself).',
    'If stability is substantially attributable to the removal of contentious politics rather than to any independent public-order improvement, the general public''s beneficiary role is overstated and the true balance tilts further toward pure extraction; if genuine improvement in predictability and safety exists independent of the suppression, the mixed beneficiary/payer role is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary_general_public, empirical, 'Whether public stability is a genuine coordination benefit or a manufactured artifact of the same suppression measured as extraction.').

omega_variable(
    retroactivity_scope_ambiguity,
    'How much of the law''s extraction operates through genuinely prospective application versus through prosecution of pre-2020 conduct recharacterized as continuing offenses?',
    'Case-level legal analysis of charging documents and judgments to classify prosecutions by whether the charged conduct occurred before or after the law''s June 2020 enactment, and whether ''continuing offense'' theories are doing substantive work.',
    'A high proportion of retroactively-reached conduct would sharpen the classification toward snare (extraction via a law applied beyond its own temporal jurisdiction); a low proportion would support a narrower, more defensible enforcement scope consistent with the sovereignty_restoration reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactivity_scope_ambiguity, empirical, 'Extent to which enforcement reaches conduct predating the law''s enactment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.37).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.39).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.85).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.87).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.9).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.91).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.92).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 60, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the single natural-language label 'the National Security Law' per the epsilon-invariance principle: this story (democratic_enclosure_reading, ε=0.88, snare-flavored, victim set = civil society/press/opposition), nsl_legal_text__jurisdictional_capture_reading (distinct ε centered on common-law/judicial autonomy erosion, distinct victim set centered on the judiciary and legal profession), and nsl_legal_text__sovereignty_restoration_reading (low ε, mountain/rope-flavored, beneficiary-coded as the general public and the state, framed as legitimate security response). All three read the identical legal text and enforcement apparatus but are structurally distinct constraints because they differ on WHO benefits, WHO pays, and what the arrangement is FOR — exactly the condition requiring decomposition rather than a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
